# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization

install.packages('tidyverse')
install.packages('lubridate')
install.packages('ggplot2')
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2) #helps visualization

getwd() #displays my working directory
setwd("/Users/yingdeng/Downloads/cyclist study/Edited CSV File") #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
oct_2021 <-read_csv("202110-divvy-tripdata-edited.csv")
nov_2021 <-read_csv("202111-divvy-tripdata-edited.csv")
dec_2021 <-read_csv("202112-divvy-tripdata-edited.csv")
jan_2022 <-read_csv("202201-divvy-tripdata-edited.csv")
feb_2022 <-read_csv("202202-divvy-tripdata-edited.csv")
mar_2022 <-read_csv("202203-divvy-tripdata-edited.csv")
apr_2022 <-read_csv("202204-divvy-tripdata-edited.csv")
may_2022 <-read_csv("202205-divvy-tripdata-edited.csv")
jun_2022 <-read_csv("202206-divvy-tripdata-edited.csv")
jul_2022 <-read_csv("202207-divvy-tripdata-edited.csv")
aug_2022 <-read_csv("202208-divvy-tripdata-edited.csv")
sep_2022 <-read_csv("202209-divvy-tripdata-edited.csv")

# Inspect parsing failures, they are all due to negative ride length or multiple-day ride length when the company took the bike out for quality inspection, so I decided it is okay to remove those data from the final analysis.
parsing_failures <- problems(oct_2021)
parsing_failures
parsing_failures2 <- problems(nov_2021)
parsing_failures2
parsing_failures3 <- problems(dec_2021)
parsing_failures3
parsing_failures4 <- problems(jan_2022)
parsing_failures4
parsing_failures5 <- problems(feb_2022)
parsing_failures5
parsing_failures6 <- problems(mar_2022)
parsing_failures6
parsing_failures7 <- problems(apr_2022)
parsing_failures7
parsing_failures8 <- problems(may_2022)
parsing_failures8
parsing_failures9 <- problems(jun_2022)
parsing_failures9
parsing_failures10 <- problems(jul_2022)
parsing_failures10
parsing_failures11 <- problems(aug_2022)
parsing_failures11
parsing_failures12 <- problems(sep_2022)
parsing_failures12

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files

colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)
colnames(may_2022)
colnames(jun_2022)
colnames(jul_2022)
colnames(aug_2022)
colnames(sep_2022)

# Inspect the dataframes and look for incongruencies
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)
str(jun_2022)
str(jul_2022)
str(aug_2022)
str(sep_2022)

# Convert date in Dec csv to date format
dec_2021 <-  mutate(dec_2021, date = as.Date(date)) 

# Stack each month's data frames into one big data frame
all_trips <- bind_rows(oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022)
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips) #list of column names
nrow(all_trips) #How many rows are in this data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of this data frame.
str(all_trips) #See list of columns and data types.
summary(all_trips) #Statistical summary of data in this data frame.

#Inspect unique values in member_casual
unique(all_trips$member_casual)

# Add a ride_length2 column toConvert "ride_length" to numeric so I can run calculations on the data.
all_trips$ride_length2 <- as.numeric((all_trips$ride_length))
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips$ride_length2)

# Compare members and casual users
aggregate(all_trips$ride_length2 ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length2 ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length2 ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length2 ~ all_trips$member_casual, FUN = min)
# Observation1: casual riders have greater mean, median and max of ride length comparing to members.

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length2 ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
# Observation2: casual riders have greater mean of ride length for each day of the week comparing to members.

# Analyze ridership data by type and weekday
all_trips %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length2,na.rm=TRUE)) %>% 
  arrange(member_casual, day_of_week)	
# Observation3: Members take more rides on weekdays compared to casual riders. Casual riders take more rides on weekends comared to members.
# Observation4: Members take more rides on weekdays compared to weekends. Casual riders take more rides on weekends compared to weekdays.

# Visualize the number of rides by rider type
all_trips %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length2,na.rm=TRUE)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week,y=number_of_rides,fill=member_casual))+geom_col(position="dodge")
# Visualize the number of rides by average duration
all_trips %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length2,na.rm=TRUE)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week,y=average_duration,fill=member_casual))+geom_col(position="dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
all_trips_tableau<- all_trips %>% 
  select(-c(start_station_name, start_station_id, end_station_name, end_station_id, started_at, ended_at,start_lat, start_lng, end_lat, end_lng))
write_csv(all_trips_tableau, file = '/Users/yingdeng/Downloads/cyclist study/Edited CSV File/trips_tableau.csv')
