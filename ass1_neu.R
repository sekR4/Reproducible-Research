
# 1. Loading and preprocessing the data----

## 1.1 Load the data ----

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "Factivity.zip", method = "auto")
unzip("Factivity.zip", exdir = "data")

dta <- read.csv("data/activity.csv")

## 1.2 Process/transform the data EDIT ----

library(tidyverse)

dta.n <- mutate(dta, date = as.Date(dta$date))
dta.n <- filter(dta.n, steps != "NA")

head(dta.n)
tail(dta.n)
str(dta.n)

# 2. What is mean total number of steps taken per day? ----

## 2.1. Calculate the total number of steps taken per day. ----

dta.n.by.day <- group_by(dta.n, date)
st.p.day <- summarise(dta.n.by.day, sum_steps = sum(steps))
st.p.day

## 2.2. Make a histogram of the total number of steps taken each day. ----

ggplot(data = st.p.day) +
        geom_histogram(mapping = aes(x = sum_steps),bins = 20, col = "blue",
                       alpha = 0.5) + 
        labs(title="Number of steps taken each day",
             x="Number of steps per day", y="Count")

## 2.3. Calculate and report the mean and median of the total number of steps taken per day ----

mean(st.p.day$sum_steps)
median(st.p.day$sum_steps)
#report ----

# 3. What is the average daily activity pattern? ----

## 3.1. Time series plot :5-minute interval and average number of steps ----

mean.st.p.int <- summarise(group_by(dta.n, interval), 
                          mean_steps = mean(steps))

ggplot(data = mean.st.p.int) +
        geom_line(mapping = aes(x = interval, y = mean_steps)) +
        labs(title="Average number of steps taken across all days",
             x="Interval", y="Average number of steps")

head(mean.st.p.int)

## 3.2 Which 5-minute interval contains the maximum number of steps in average? EDIT----

max5 <- mean.st.p.int[which.max(mean.st.p.int$mean_steps),]
max5

# 4. Imputing missing values ----

## 4.1. Calculate and report the total number of missing values in the dataset ---- 

NAs <- nrow(dta[which(is.na(dta)==TRUE),])
NAs
# report----
## 4.2. Devise a strategy for filling in all of the missing values in the dataset.----

m.st.p.int <- summarise(group_by(dta,interval), 
                        mean_steps = mean(steps, na.rm = TRUE))

## 4.3. Create a new dataset with the missing data filled in. ----

dta.sim <- dta
for(r in 1:nrow(dta.sim)){
        if (is.na(dta.sim$steps[r])) {
                st.all <- m.st.p.int$mean_steps[
                        m.st.p.int$interval == dta.sim$interval[r]];
                dta.sim$steps[r] <- st.all}}

summary(dta.sim)
str(dta.sim)

## 4.4. Make a histogram, mean and median of steps taken per day. ----

dta.by.date.sim <- group_by(dta.sim, date)

st.p.day.sim <- summarise(group_by(dta.sim, date), sum_steps = sum(steps))

ggplot(data = st.p.day.sim) +
        geom_histogram(mapping = aes(x = sum_steps), bins = 20,
                       col = "blue", alpha = 0.5) +
        labs(title="Number of steps taken each day", subtitle="With imputed NA's",
             x="Number of steps per day", y="Count")

# Mean and Median with replaced NA's
mean(st.p.day.sim$sum_steps); median(st.p.day.sim$sum_steps) # Mean = Median

# Mean and Median without NA's
mean(st.p.day$sum_steps); median(st.p.day$sum_steps) # Small difference between Mean and Median


# 5. Are there differences in activity patterns between weekdays and weekends? ----

## 5.1. Create a "weekday"/"weekend"-factor ----

dta.sim$date <- as.Date(dta.sim$date)
dta.sim <- mutate(dta.sim, wd = weekdays(date))
dta.sim <- dta.sim %>%
        mutate(wd.we = as.factor(ifelse(wd %in% c("Saturday", "Sunday") == TRUE,
                                        "weekend", "weekday")))

## 5.2. Make a time series panel plot: 5-minute interval and average number of steps ----

panel.dta <- summarise(
        group_by(dta.sim, wd.we,interval), 
        mean(steps))

library(lattice)

with (panel.dta, 
      xyplot(`mean(steps)`~ interval|wd.we, type="l", 
             xlab = "Interval",
             ylab="Number of steps",
             layout=c(1,2)))

