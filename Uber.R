rm(list=ls())
uber_data <-read.csv("Uber Request Data.csv")
#Checking for duplicate rows
sum(duplicated(uber_data$Request.id))
#Checking for empty values
sapply(uber_data, function(x) length(which(x == "" || x == " " || x == 0)))
#Analysing the six columns
summary(uber_data$Pickup.point)
summary(uber_data$Request.id)
summary(uber_data$Driver.id)
##2650-No Cars Available, 1264-Cancelled
summary(uber_data$Status)
#No NA values in Request.timestamp
sum(is.na(uber_data$Request.timestamp))
# 3914 NA values in Drop.timestamp corresponding to Drop.timestamp = Cancelled + No Cars Available exact match
sum(is.na(uber_data$Drop.timestamp))
#convert the Request.timestamp to a single fromat 
install.packages("lubridate")
library(lubridate)
request_datetime <- parse_date_time(uber_data$Request.timestamp,orders= c("%d-%m-%Y %H:%M:%S" , "%d/%m/%Y %H:%M"))
uber_data <- cbind(uber_data,request_datetime)
#convert the Drop.timestamp to a single format
drop_datetime <- parse_date_time(uber_data$Drop.timestamp,orders= c("%d-%m-%Y %H:%M:%S" , "%d/%m/%Y %H:%M"))
uber_data <- cbind(uber_data,drop_datetime)
#Trip time , a new column for the minutes of travel
trip_time <- as.integer(difftime(drop_datetime,request_datetime,units="mins"))
uber_data <- cbind(uber_data,trip_time)
boxplot(uber_data$trip_time)
###MEDIAN trip time = 52 minutes
#extracting the request time and drop time from date time
request_time <- strftime(request_datetime,format="%H:%M:%S")
drop_time <- strftime(drop_datetime,format="%H:%M:%S")
uber_data <- cbind(uber_data,request_time,drop_time)
#Creating time intervals for request times
request_time_interval <- cut.POSIXt(as.POSIXct(uber_data$request_time,format="%H:%M:%S"),8,labels = c("midnight","early morning","morning","late morning","afternoon","evening","night","late night"))
drop_time_interval <- cut.POSIXt(as.POSIXct(uber_data$drop_time,format="%H:%M:%S"),8,labels = c("midnight","early morning","morning","late morning","afternoon","evening","night","late night"))
uber_data <- cbind(uber_data,request_time_interval,drop_time_interval)
#Frequency plots for trips
install.packages("ggplot2")
library(ggplot2)
ggplot (uber_data,aes(x=request_time_interval,fill=Status)) + geom_bar(stat="count",position="Stack")  + facet_grid(~Pickup.point)
supply <- uber_data$Status == "Trip Completed"
uber_data <- cbind(uber_data,supply)
ggplot (uber_data,aes(x=request_time_interval,fill=supply)) + geom_bar(stat="count",position="Stack")
ggplot (uber_data,aes(x=request_time_interval,fill=supply)) + geom_bar(stat="count",position="fill")
ggplot (uber_data,aes(x=request_time_interval,fill=supply)) + geom_bar(stat="count",position="Stack")  + facet_grid(~Pickup.point)
ggplot (uber_data,aes(x=request_time_interval,fill=supply)) + geom_bar(stat="count",position="fill")  + facet_grid(~Pickup.point)
uber_data$supply <- as.integer(uber_data$supply)
install.packages("dplyr")
library("dplyr")
uber_data_time_group <- group_by(uber_data,request_time_interval,Pickup.point)
uber_data_time_group_summary <- summarise(uber_data_time_group,demand=n(),supply=sum(supply),completion_ratio_percentage=sum(supply)/n()*100,gap=demand-supply)
#Plotting the demand-supply gap as a function oftime in airport and city
ggplot(uber_data_time_group_summary,aes(y=gap,x=request_time_interval))+geom_bar(stat="identity") + facet_grid(~Pickup.point)
ggplot(uber_data_time_group_summary,aes(y=demand,x=request_time_interval))+geom_bar(stat="identity") + facet_grid(~Pickup.point)
ggplot(uber_data_time_group_summary,aes(y=supply,x=request_time_interval))+geom_bar(stat="identity") + facet_grid(~Pickup.point)

###Waiting time

uber_data_airport <- filter(uber_data,Pickup.point=="Airport")
uber_data_city <- filter(uber_data,Pickup.point=="City")
uber_data_airport <- arrange(uber_data_airport,request_datetime)
uber_data_city <- arrange(uber_data_city,request_datetime)

time_between_requests <- difftime(time1 = lead(uber_data_airport$request_datetime),time2 = uber_data_airport$request_datetime, units = 'hours')
uber_data_airport <- cbind(uber_data_airport,time_between_requests)
#boxplot(uber_data_airport$time_between_requests)
ggplot(uber_data_airport,aes(y=as.numeric(time_between_requests),x=request_time)) + geom_point()
time_between_requests <- difftime(time1 = lead(uber_data_city$request_datetime),time2 = uber_data_city$request_datetime, units = 'hours')
uber_data_city <- cbind(uber_data_city,time_between_requests)
ggplot(uber_data_city,aes(y=as.numeric(time_between_requests),x=request_time)) + geom_point()
#boxplot(uber_data_city$time_between_requests)
