## Tejas Athni
## History 181B - Final Project


# Setup
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(leaflet)
library(geosphere)
library(formattable)
setwd("E:/SynologyDrive/Tejas_Server/! Stanford University/! Year 3 - Quarter 2/History 181B/Final Project/")


# Load in the Turjman dataset of geo-observations
data <- read_excel("Turjman_Data.xlsx")


# Map of all observations in aggregate, with summary statistics
all_data <- data
leaflet(data = all_data) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))

all_num <- nrow(all_data)
all_physical <- nrow(all_data %>% filter(Physical1_or_Mental0 == 1))
all_mental <- nrow(all_data %>% filter(Physical1_or_Mental0 == 0))
all_maxDist <- (geosphere::distm(c(35.227614, 31.77661), c(78.96288, 20.59368), fun = distHaversine)[[1]] / 1000) %>%
  round(digits = 1)

all_matrix <- c(all_num, all_physical, all_mental, "India", all_maxDist)
all_summary <- as.data.frame(matrix(data = all_matrix, nrow = 1, ncol = 5, byrow = T))
colnames(all_summary) <- c("All Locations", "Physically Visited", "Mentally Referenced", "Place of Max Distance", "Kilometers of Max Distance")

formattable(all_summary, 
            align =c("c","c","c","c","c"))



# Map of physical observations only
physical_data <- data %>% filter(Physical1_or_Mental0 == 1)
leaflet(data = physical_data) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))


# Map of mental observations only
mental_data <- data %>% filter(Physical1_or_Mental0 == 0)
leaflet(data = mental_data) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))




# Stratify dataset by month of location occurrence

mar15 <- data[c(1:11),]
apr15 <- data[c(12:43),]
may15 <- data[c(44:78),]
sep15 <- data[c(79:101),]
oct15 <- data[c(102:107),]

jan16 <- data[c(108:112),]
feb16 <- data[c(113:116),]
apr16 <- data[c(117:117),]
may16 <- data[c(118:121),]
jul16 <- data[c(122:129),]
aug16 <- data[c(130:130),]



# Histogram figure of location occurrences by calendar time in '15 and '16
histogram_data <- data.frame(c("Jan 1915","Feb 1915","Mar 1915","Apr 1915","May 1915","June 1915",
                               "July 1915","Aug 1915","Sep 1915","Oct 1915","Nov 1915","Dec 1915",
                               "Jan 1916","Feb 1916","Mar 1916","Apr 1916","May 1916","June 1916",
                               "July 1916","Aug 1916","Sep 1916","Oct 1916","Nov 1916","Dec 1916"),
                             c(0,0,nrow(mar15),nrow(apr15),nrow(may15),0,0,0,nrow(sep15),nrow(oct15),0,0,
                               nrow(jan16),nrow(feb16),0,nrow(apr16),nrow(may16),0,nrow(jul16),nrow(aug16),0,0,0,0))
colnames(histogram_data) <- c("Month_Year", "Number_of_Location_References")
histogram_data$Month_Year <- factor(histogram_data$Month_Year, levels = histogram_data$Month_Year[order(c(24:1))])



ggplot(data = histogram_data, aes(x = Month_Year, y = Number_of_Location_References)) +
  coord_flip() +
  geom_col(alpha=0.85)




# Maps stratified by the 4 clusters
cluster1 <- data[c(1:78),]
cluster2 <- data[c(79:107),]
cluster3 <- data[c(108:116),]
cluster4 <- data[c(117:130),]


leaflet(data = cluster1) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))

cluster1_num <- nrow(cluster1)
cluster1_physical <- nrow(cluster1 %>% filter(Physical1_or_Mental0 == 1))
cluster1_mental <- nrow(cluster1 %>% filter(Physical1_or_Mental0 == 0))
cluster1_maxDist <- (geosphere::distm(c(35.227614, 31.77661), c(2.35222, 48.85661), fun = distHaversine)[[1]] / 1000) %>%
  round(digits = 1)

cluster1_matrix <- c(cluster1_num, cluster1_physical, cluster1_mental, "Paris, France", cluster1_maxDist)
cluster1_summary <- as.data.frame(matrix(data = cluster1_matrix, nrow = 1, ncol = 5, byrow = T))
colnames(cluster1_summary) <- c("All Locations", "Physically Visited", "Mentally Referenced", "Place of Max Distance", "Kilometers of Max Distance")

formattable(cluster1_summary, 
            align =c("c","c","c","c","c"))




leaflet(data = cluster2) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))

cluster2_num <- nrow(cluster2)
cluster2_physical <- nrow(cluster2 %>% filter(Physical1_or_Mental0 == 1))
cluster2_mental <- nrow(cluster2 %>% filter(Physical1_or_Mental0 == 0))
cluster2_maxDist <- (geosphere::distm(c(35.227614, 31.77661), c(2.35222, 48.85661), fun = distHaversine)[[1]] / 1000) %>%
  round(digits = 1)

cluster2_matrix <- c(cluster2_num, cluster2_physical, cluster2_mental, "Paris, France", cluster2_maxDist)
cluster2_summary <- as.data.frame(matrix(data = cluster2_matrix, nrow = 1, ncol = 5, byrow = T))
colnames(cluster2_summary) <- c("All Locations", "Physically Visited", "Mentally Referenced", "Place of Max Distance", "Kilometers of Max Distance")

formattable(cluster2_summary, 
            align =c("c","c","c","c","c"))




leaflet(data = cluster3) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))

cluster3_num <- nrow(cluster3)
cluster3_physical <- nrow(cluster3 %>% filter(Physical1_or_Mental0 == 1))
cluster3_mental <- nrow(cluster3 %>% filter(Physical1_or_Mental0 == 0))
cluster3_maxDist <- (geosphere::distm(c(35.227614, 31.77661), c(39.56918, 24.52465), fun = distHaversine)[[1]] / 1000) %>%
  round(digits = 1)

cluster3_matrix <- c(cluster3_num, cluster3_physical, cluster3_mental, "Medina", cluster3_maxDist)
cluster3_summary <- as.data.frame(matrix(data = cluster3_matrix, nrow = 1, ncol = 5, byrow = T))
colnames(cluster3_summary) <- c("All Locations", "Physically Visited", "Mentally Referenced", "Place of Max Distance", "Kilometers of Max Distance")

formattable(cluster3_summary, 
            align =c("c","c","c","c","c"))




leaflet(data = cluster4) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~as.character(Location_Address), label= ~as.character(Location_Address),
             labelOptions = labelOptions(noHide = T))

cluster4_num <- nrow(cluster4)
cluster4_physical <- nrow(cluster4 %>% filter(Physical1_or_Mental0 == 1))
cluster4_mental <- nrow(cluster4 %>% filter(Physical1_or_Mental0 == 0))
cluster4_maxDist <- (geosphere::distm(c(35.227614, 31.77661), c(78.96288, 20.59368), fun = distHaversine)[[1]] / 1000) %>%
  round(digits = 1)

cluster4_matrix <- c(cluster4_num, cluster4_physical, cluster4_mental, "India", cluster4_maxDist)
cluster4_summary <- as.data.frame(matrix(data = cluster4_matrix, nrow = 1, ncol = 5, byrow = T))
colnames(cluster4_summary) <- c("All Locations", "Physically Visited", "Mentally Referenced", "Place of Max Distance", "Kilometers of Max Distance")

formattable(cluster4_summary, 
            align =c("c","c","c","c","c"))



# Plot occurrences by count

data_count <- data %>% mutate(count = 1)
data_count %<>% group_by(Location_Address) %>% summarize(mentions = sum(count))
data_count$Location_Address <- factor(data_count$Location_Address, levels = data_count$Location_Address[rev(order(data_count$mentions))])

ggplot(data = data_count, aes(x = Location_Address, y = mentions)) +
  coord_flip() +
  geom_col(alpha=0.8,col="grey", fill="dodgerblue4") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))


