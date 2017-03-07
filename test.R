
#  Reproducible Research, Peer Assessment 2
#  Storm Threat Analysis - Test File
#  MAC, 2/26/2017


## 1. Data Processing
##---------------------------------------

# library(R.utils)
library(plyr)
library(dplyr)

# Download data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","data/StormData.csv.bz2")

# Unzip data file
bunzip2("data/StormData.csv.bz2", "data/StormData.csv", remove = F, skip = T)

# Read column header
h1 <- readLines("data/StormData.csv",1)

# Remove uneeded characters from header
h1 <- gsub("\"","",h1)

# Create a list from row names
h1 <- strsplit(h1,',',fixed = T)

# Read in rest of data file
sdata <- read.csv("data/StormData.csv", skip = 1, header = F)

# Set column names
names(sdata) <- make.names(h1[[1]])
rm(h1)

## 2. Population Health (PH) Impacts
##----------------------------------

# Extract "event type" (col 8), "fatalities" (col 23), and 
# "injuries" (col 24) from storm data table
ph <- sdata[,c(8,23,24)]

# Group the storm data by "event type"
byGrp <- group_by(ph,EVTYPE)

# Summarize by total number of fatalities and injuries by event, 
# and average fatalities and injuries per event. 
phSum <- summarize(byGrp, COUNT = n(), FATALITY = sum(FATALITIES), INJURY = sum(INJURIES), FATAL_PER_EVT = FATALITY/COUNT, INJURE_PER_EVT = INJURY/COUNT)

# Arrange storm data in descending order by number 
# of fatalities associated with each event
descFatal <- arrange(phSum, desc(FATALITY))

# Filter out non-standard naming conventions used in reporting 
# reporting weather event types
phSum2 <- filter(phSum, COUNT > 100)

# Arrange storm data in descending order by average 
# fatalities associated with each event
descAvFatal <- arrange(phSum2, desc(FATAL_PER_EVT))

rm(phSum, phSum2)


## 3. Property Damage Impacts
##---------------------------


# Create function to convert alphabetic loss figures to numeric values
magTimes <- function(x, y) {
  if (y=="K"|y=="k") {
    result <- x * 1000;
  } else if (y=="M"|y=="m") {
    result <- x * 1000000;
  } else if (y=="B"|y=="b") {
    result <- x * 1000000000;
  } else {
    result <- x;
  }
  return(result)  
}

# Add new column "propdmgamt" to property damage table
# to create numeric loss figures
pd$propdmgamt <- mapply(magTimes,pd$PROPDMG,pd$PROPDMGEXP)

# Group the storm data by "event type"
byGrp <- group_by(pd,EVTYPE)

# Summarize by total property damage loss associated with the event type, and
# average property damage per event 
pdSum <- summarize(byGrp, COUNT = n(), PROP_DMG_AMT = sum(propdmgamt), PROP_DMG_PER_EVT = PROP_DMG_AMT/COUNT)

# Arrange storm data in descending order by property 
# damage loss associated with each event
descPropLoss <- arrange(pdSum, desc(PROP_DMG_AMT))
rm(pdSum)

## 4. Crop Damage (CD) Impacts
##---------------------------

# Extract "event type" (col 8), "crop damage" (col 27), 
# and "crop damage magnitude (col 28)  from storm data table 
cd <- sdata[,c(8,27:28)]

# Add new column "cropdmgamt" to crop damage table
# to create numeric loss figures
cd$cropdmgamt <- mapply(magTimes,cd$CROPDMG,cd$CROPDMGEXP)

# Group the storm data by "event type"
byGrp <- group_by(cd,EVTYPE)

# Summarize by total crop damage loss associated with each event, 
# and average loss reported per event  
cdSum <- summarize(byGrp, COUNT = n(), CROP_DMG_AMT = sum(cropdmgamt), CROP_DMG_PER_EVT = CROP_DMG_AMT/COUNT)

# Arrange storm data in descending order by total crop 
# damage loss associated with each event
descCropLoss <- arrange(cdSum, desc(CROP_DMG_AMT))
rm(cdSum)


## 5. Results
##-----------

# Load libraries
library(knitr)

# Summarize Top 10 Event Types by total fatalities and injuries reported
kable(head(descFatal[,c(1,3:4)],10),  caption = "Table 1: Total Reported Fatalities and Injuries by Event Type", col.names = c("Event Type","Fatalities","Injuries"))

# Summarize Top 10 Event Types by average fatalities and injuries per event
kable(head(descAvFatal[,c(1,5:6)],10),  caption = "Table 2: Average Fatalities and Injuries per Event", col.names = c("Event Type","Average Fatalities","Average Injuries"))


# Plots : Figure 1, Total Prop Loss
#----------------------------------

# par(mfrow = c(1,1))
options(scipen = 999)

# Calculate top 10 reported event types
top10propLoss <- head(descPropLoss,10)

# Refactor the event type to 10 returned factors
top10propLoss$EVTYPE <- factor(top10propLoss$EVTYPE)

# Create a boxplot of the total number of property loss incidents reported
# boxplot(top10propLoss$COUNT ~ top10propLoss$EVTYPE, cex.axis = 0.4, mar = c(5,2,2,10) + 0.1, las = 2, main = "Total Property Loss Events Reported")

# Set axis label orientation and font size
par(las = 2, cex.axis = 0.4)

# Create a barplot of the total number of property loss incidents reported
barplot(top10propLoss$COUNT, names.arg = top10propLoss$EVTYPE, ylim=c(0,300000), main = "Total Property Loss Events Reported")

# Plots : Figure 2, Avg Prop Loss 
#-------------------------------
# par(mfrow = c(1,2))

# Set axis label orientation and font size
par(las = 2, cex.axis = 0.4)

# Create barplot of top 10, average property loss per event type
barplot(top10propLoss$PROP_DMG_PER_EVT, names.arg = top10propLoss$EVTYPE, ylim=c(0,800000000), main = "Top 10 Average Property Loss per Event Type")


# Plots : Figure 3, Avg Crop Loss
#--------------------------------

# Calculate top 10 reported event types
top10cropLoss <- head(descCropLoss,10)

# Refactor the event type to 10 returned factors
top10cropLoss$EVTYPE <- factor(top10cropLoss$EVTYPE)

# Set axis label orientation and font size
par(las = 2, cex.axis = 0.4)

# Create barplot of top 10, average crop loss per event type
barplot(top10cropLoss$CROP_DMG_PER_EVT, names.arg = top10cropLoss$EVTYPE, ylim=c(0,30000000), main = "Top 10 Average Crop Loss per Event Type")




