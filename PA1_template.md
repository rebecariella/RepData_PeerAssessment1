Loading and preprocessing the data
----------------------------------

    url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    setwd("C:/Users/rriella/Desktop/Curso Coursera Data Science/5. Reproducible Research")
    temp = tempfile()
    download.file(url, temp,mode="wb")
    unzip(temp, "activity.csv")
    base = read.table("activity.csv", sep=",", header=T)

    str(base)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
-------------------------------------------------

Total number of steps taken per day

    library(tidyverse)
    base %>% 
      group_by(date) %>% 
      summarise(total = sum(steps,na.rm=T)) %>% head()

    ## # A tibble: 6 x 2
    ##   date       total
    ##   <fct>      <int>
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

Histogram of the total number of steps taken each day

    plot1 = base %>% 
      group_by(date) %>% 
      summarise(total = sum(steps,na.rm=T)) %>% 
      ggplot(aes(x=total))+geom_histogram(col="white", fill="blue", alpha=0.3)+
      labs(x= "Total number of steps taken per day", title="Plot 1: Histogram of the number of steps taken each day")
    plot1

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Mean and median of the total number of steps taken per day

    a = base %>% 
      group_by(date) %>% 
      summarise(total = sum(steps,na.rm=T)) %>% 
      summarise(mean = mean(total,na.rm=T),
                median = median(total,na.rm=T))
    knitr::kable(a)

<table>
<thead>
<tr class="header">
<th align="right">mean</th>
<th align="right">median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">9354.23</td>
<td align="right">10395</td>
</tr>
</tbody>
</table>

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    base %>% 
      group_by(interval) %>% 
      summarise(mean = mean(steps,na.rm=T)) %>% 
        ggplot(aes(x=interval, y=mean))+geom_line(col="blue", alpha=0.3, lwd=1.3)+
      labs(title = "Plot 2. Average number of steps taken of each 5-minute interval",
           x = "Time")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    b= base %>% 
      group_by(interval) %>% 
      summarise(mean = mean(steps,na.rm=T)) %>% slice_max(mean)
    knitr::kable(b)

<table>
<thead>
<tr class="header">
<th align="right">interval</th>
<th align="right">mean</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">835</td>
<td align="right">206.1698</td>
</tr>
</tbody>
</table>

Imputing missing values
-----------------------

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

    base %>% summarise(total_NA = sum(is.na(steps)))

    ##   total_NA
    ## 1     2304

Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

Strategy: I fill all the missing values in the dataset using the mean
for that 5-minute interval.

Create a new dataset that is equal to the original dataset but with the
missing data filled in.

    base_woNA = base %>% 
      group_by(interval) %>% 
        mutate(steps_interval = mean(steps,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(steps = ifelse(is.na(steps)==T,steps_interval,steps))

Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day.

    plot3 = base_woNA %>% 
      group_by(date) %>% 
      summarise(total = sum(steps,na.rm=T)) %>% 
      ggplot(aes(x=total))+geom_histogram(col="white", fill="green", alpha=0.3)+
      labs(x= "Total number of steps taken per day", 
         title="Plot 3: Histogram of the number of steps taken each day. NA imputed")
    plot3

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    a = base_woNA %>% 
      group_by(date) %>% 
      summarise(total = sum(steps,na.rm=T)) %>% 
      summarise(mean = mean(total,na.rm=T),
                median = median(total,na.rm=T))
    knitr::kable(a)

<table>
<thead>
<tr class="header">
<th align="right">mean</th>
<th align="right">median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">10766.19</td>
<td align="right">10766.19</td>
</tr>
</tbody>
</table>

    library(cowplot)

    plot_grid(nrow = 2, 
    plot1,
    plot3,
    labels = "Plot 4")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Do these values differ from the estimates from the first part of the
assignment? What is the impact of imputing missing data on the estimates
of the total daily number of steps?

**Answer:** The main differences are:

-   There are not as much 0

-   There are more counts in the middle of the distribution

-   Mean and Median are equal

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    library(lubridate)
    base %>% 
      mutate(
        date = as.Date(date,"%Y-%m-%d"),
        wday =wday(date),
        weekdays = ifelse(wday %in% c(2:6),"weekday","weekend")) %>% 
         group_by(interval,weekdays) %>% 
      summarise(mean = mean(steps,na.rm=T)) %>% ungroup() %>% 
        ggplot(aes(x=interval, y=mean))+geom_line(col="blue", alpha=0.3, lwd=1.3)+facet_grid(weekdays~.)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

knit2html()
