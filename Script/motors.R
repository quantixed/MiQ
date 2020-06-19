require(XML)
require(ggplot2)
require(cowplot)

## Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Data"), dir.create("Output/Data"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Script"), dir.create("Script"), "Folder exists already")

filename <- "Data/pubmed_result.xml"
## extract a data frame from XML file
## This is modified from christopherBelter's pubmedXML R code
extract_xml <- function(theFile) {
  library(XML)
  newData <- xmlParse(theFile)
  records <- getNodeSet(newData, "//PubmedArticle")
  pmid <- xpathSApply(newData,"//MedlineCitation/PMID", xmlValue)
  doi <- lapply(records, xpathSApply, ".//ELocationID[@EIdType = \"doi\"]", xmlValue)
  doi[sapply(doi, is.list)] <- NA
  doi <- unlist(doi)
  authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
  authLast[sapply(authLast, is.list)] <- NA
  authInit <- lapply(records, xpathSApply, ".//Author/Initials", xmlValue)
  authInit[sapply(authInit, is.list)] <- NA
  authors <- mapply(paste, authLast, authInit, collapse = "|")
  affiliations <- lapply(records, xpathSApply, ".//Author/AffiliationInfo/Affiliation", xmlValue)
  affiliations[sapply(affiliations, is.list)] <- NA
  affiliations <- sapply(affiliations, paste, collapse = "|")
  year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
  year[sapply(year, is.list)] <- NA
  year <- unlist(year)
  articletitle <- lapply(records, xpathSApply, ".//ArticleTitle", xmlValue) 
  articletitle[sapply(articletitle, is.list)] <- NA
  articletitle <- unlist(articletitle)
  journal <- lapply(records, xpathSApply, ".//ISOAbbreviation", xmlValue) 
  journal[sapply(journal, is.list)] <- NA
  journal <- unlist(journal)
  volume <- lapply(records, xpathSApply, ".//JournalIssue/Volume", xmlValue)
  volume[sapply(volume, is.list)] <- NA
  volume <- unlist(volume)
  issue <- lapply(records, xpathSApply, ".//JournalIssue/Issue", xmlValue)
  issue[sapply(issue, is.list)] <- NA
  issue <- unlist(issue)
  pages <- lapply(records, xpathSApply, ".//MedlinePgn", xmlValue)
  pages[sapply(pages, is.list)] <- NA
  pages <- unlist(pages)
  abstract <- lapply(records, xpathSApply, ".//Abstract/AbstractText", xmlValue)
  abstract[sapply(abstract, is.list)] <- NA
  abstract <- sapply(abstract, paste, collapse = "|")
  recdatey <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'received']/Year", xmlValue)
  recdatey[sapply(recdatey, is.list)] <- NA
  recdatem <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'received']/Month", xmlValue)
  recdatem[sapply(recdatem, is.list)] <- NA
  recdated <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'received']/Day", xmlValue)
  recdated[sapply(recdated, is.list)] <- NA
  recdate <- mapply(paste, recdatey, recdatem, recdated, collapse = "|")
  accdatey <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'accepted']/Year", xmlValue)
  accdatey[sapply(accdatey, is.list)] <- NA
  accdatem <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'accepted']/Month", xmlValue)
  accdatem[sapply(accdatem, is.list)] <- NA
  accdated <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'accepted']/Day", xmlValue)
  accdated[sapply(accdated, is.list)] <- NA
  accdate <- mapply(paste, accdatey, accdatem, accdated, collapse = "|")
  # use pubmed date as the published date. This seems safe for older records.
  pubdatey <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'pubmed']/Year", xmlValue)
  pubdatey[sapply(pubdatey, is.list)] <- NA
  pubdatem <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'pubmed']/Month", xmlValue)
  pubdatem[sapply(pubdatem, is.list)] <- NA
  pubdated <- lapply(records, xpathSApply, ".//PubMedPubDate[@PubStatus = 'pubmed']/Day", xmlValue)
  pubdated[sapply(pubdated, is.list)] <- NA
  pubdate <- mapply(paste, pubdatey, pubdatem, pubdated, collapse = "|")
  ptype <- lapply(records, xpathSApply, ".//PublicationType", xmlValue)
  ptype[sapply(ptype, is.list)] <- NA
  ptype <- sapply(ptype, paste, collapse = "|")
  theDF <- data.frame(pmid, doi, authors, affiliations, year, articletitle, journal, volume, issue, pages, abstract, recdate, accdate, pubdate, ptype, stringsAsFactors = FALSE)
  ## convert the dates
  theDF$recdate <- as.Date(theDF$recdate, format="%Y %m %d")
  theDF$accdate <- as.Date(theDF$accdate, format="%Y %m %d")
  theDF$pubdate <- as.Date(theDF$pubdate, format="%Y %m %d")
  return(theDF)
}

## make the dataframe
theData <- extract_xml(filename)
## have a look at a few titles
theData[sample(nrow(theData), 5), "articletitle"]

## now we extract the country from the last authors affiliation
theData$country <- gsub(".*, (.*)","\\1",theData$affiliations)
## remove last period
theData$country <- sub(".$", "", theData$country)
## these countries need to be cleaned up
## load country lookup
country_lookup <- read.table("Data/country_lookup.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
for (row in 1:nrow(country_lookup)) { 
  regex <- country_lookup$Regex[row]
  theCountry <- country_lookup$Replace[row]
  theData$country <- gsub(regex, theCountry, theData$country) 
}
## save out the data
write.table(theData, file = "Output/Data/pubmed_data.txt", sep = "\t", row.names = F)

## prepare to plot the data
countryDF <- as.data.frame(table(theData$country))
names(countryDF) <- c("Country","Count")
countryDF <- subset(countryDF, Country != "N")
countryDF <- subset(countryDF, Count >= 10)
countryDF$Country <- factor(countryDF$Country,
                  levels = countryDF$Country[order(countryDF$Count, decreasing = TRUE)])
countryDF$Fraction <- countryDF$Count / sum(countryDF$Count,na.rm = T)

## load MiQ data
miqDF<- read.table("Data/miq.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
miqDF$Country <- factor(miqDF$Country,
                        levels = miqDF$Country[order(miqDF$Attendees, decreasing = TRUE)])
miqDF$Fraction <- miqDF$Attendees / sum(miqDF$Attendees,na.rm = T)

## merge the two dataframes
compareDF <- merge(countryDF, miqDF, by="Country")

## make the plots
p1 <- ggplot(data = countryDF, aes(x = Country, y = Count)) +
  geom_bar(stat = "identity") +
  labs(y = "Papers") +
  theme_half_open(12) +
  background_grid() +
  coord_flip()
ggsave("Output/Plots/papers_per_country.png", p1, dpi = 300, width = 170, height = 100, units = "mm")

p2 <- ggplot(data = miqDF, aes(x = Country, y = Attendees)) +
  geom_bar(stat = "identity") +
  labs(y = "Average attendees") +
  theme_half_open(12) +
  background_grid() +
  coord_flip()
ggsave("Output/Plots/attendees_per_country.png", p2, dpi = 300, width = 170, height = 100, units = "mm")

p3 <- plot_grid(p1, p2)
ggsave("Output/Plots/combined.png", p3, dpi = 300)

p4 <- ggplot(data = countryDF, aes(x = Country, y = Fraction)) +
  geom_bar(stat = "identity") +
  labs(y = "Papers fraction") +
  theme_half_open(12) +
  background_grid() +
  coord_flip()
ggsave("Output/Plots/paperfraction_per_country.png", p4, dpi = 300, width = 170, height = 100, units = "mm")

p5 <- ggplot(data = miqDF, aes(x = Country, y = Fraction)) +
  geom_bar(stat = "identity") +
  labs(y = "Attendee fraction") +
  theme_half_open(12) +
  background_grid() +
  coord_flip()
ggsave("Output/Plots/attendeefraction_per_country.png", p5, dpi = 300, width = 170, height = 100, units = "mm")

p6 <- plot_grid(p4, p5)
ggsave("Output/Plots/combined_fraction.png", p6, dpi = 300)

p7 <- ggplot(compareDF,
             aes(x = Fraction.x, y = Fraction.y, colour = Country, alpha = 0.5)) +
  geom_point(aes(size = 1.5)) +
  guides(size = FALSE, alpha = FALSE, colour = guide_legend(override.aes = list(alpha = 0.5))) +
  xlim(c(0,0.4)) + ylim(c(0,0.4)) +
  labs(x = "Papers fraction", y = "Attendees fraction") +
  theme_half_open(12) +
  background_grid()
ggsave("Output/Plots/compare_fraction.png", p7, dpi = 300, width = 140, height = 100, units = "mm")
