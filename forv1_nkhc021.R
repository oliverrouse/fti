#### Pakker ####
library(tidyverse)
library(dkstat)

#### Realvækst NKHC021 ####
## Hent forbrugsdata via DST API
Forbrug.meta <- dst_meta(table="NKHC021", lang="da")

Forbrug.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*")

Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrug.filter)
Forbrugdata <- Forbrugdata[,-c(1:3)]

## Udregner den kvartalvise årlige realvæskt
Forbrugdata$Realvækst <- (Forbrugdata$value / dplyr::lag(Forbrugdata$value, 4) - 1) * 100

Privatforbrug.periode <- as.data.frame(Forbrugdata[41:nrow(Forbrugdata),c(1,3)]) #tager for den udvalgte periode
rownames(Privatforbrug.periode) <- NULL

#### Forbrugerforvetninger FORV1 ####
FORV1.meta <- dst_meta(table = "FORV1", lang = "da")

## Liste med relevante filter-variabler.
FORV1.filter <- list(
  INDIKATOR = "*",
  Tid = "*")

FORV1 <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")
FORV1 <- pivot_wider(FORV1, names_from = INDIKATOR, values_from = value)

## Gruppering og opsummering med udregning af mean
FORV1$TID <- paste(year(FORV1$TID), " Q", quarter(FORV1$TID), sep = "")

FORV1 <- FORV1 %>%
  group_by(TID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## Skær til fra 2000Q1
FORV1 <- FORV1[-1:-101,]


#### Dataframe med FTI og Realvækst i lige lange mulige perioder ####
FTI <- data.frame(Privatforbrug.periode)
FTI <- cbind(FTI, FORV1[1:nrow(FTI),-1])
