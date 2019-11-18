
########################### USING CEMETERY DATA IN R ######################


##########Load the .csv files into rStudio#########

#First, set a "working directory" to make it easier to find files etc. 

#setwd(##put your folder address from the C drive) or [easy method] use the 
#command in the "session" toolbar above to set working directory

#download the .CSV files 1850 and 1880, and cemdata.xlsx to your desktop or a working directory folder

#Click "import dataset" on the "environment" tab, choose one, repeat steps for the other.
#your workbook (this pane) will open each .csv file to view, use the tabs above to come back 
#to these instructions. You can also (after setting WD) type the commands below.

##################DPYLER#################

#import xlxs
library(readxl)
cemdata <- read_excel("cemdata.xlsx")
library(dplyr)
alt190M <- filter(cemdata, Cohort==190, Sex=="M")


cemdata_age <- mutate(cemdata, age=(cemdata$`Death Year`)-(cemdata$`Birth Year`))





X1850 <- read.csv("1850.csv")


X1880 <- read.csv("1880.csv")



#the X shows up because we can't name a data frame a number. 

#now split the data frame 1850 into females and males. couple notes here:
#the square brackets are used for subsetting data frames (like X1850) into smaller bunches
#note the comma space after the row numbers... you can tell it which columns to subset too
#[1:3, ] gives all columns in rows one to three
#[ ,1:3] subsets all rows and just columns one to three
#subset function is [rows, columns]
#use simple names for new DF, but make sure they are descriptive enough. F50 is 
#females from the 1850 cohort. You could use X and Y but you'd forget WTH they mean

F50<-X1850 [2:392, ]
M50<-X1850 [393:770, ]

#awesome, you’ve read in and subsetted your data. You've done more than most people ever can 
#in this godforsaken program. 

F80 <- X1880 [2:725, ]
M80 <- X1880 [726:1494, ]

###### Now, Behold the POWER #######

#to look at one column, use (data frame$column name)
#try just looking at females in 1850's cohort age at death

print(F50$A)

#Okay, great. Now what? #let's put them in order. 

ageF50 <- sort(F50$A)

#awesome, sorted. 
#but now it is a "value" and not with the other stuff. That's okay, tell R to change the 
#type of data that it is!

ageF50 <- as.data.frame(ageF50)

#this moves it back upstairs. 

#How about some basic stats on the females before we start making pretty pictures?

summary(ageF50)

#lets compare men and women's lifespan from the 1850s
#we need to manipulate our data in the same way

ageM50 <- (M50$A)

ageM50 <-sort(ageM50)

ageM50 <- as.data.frame (ageM50)

#okay, read in. Now, let's see a summary. 

summary (ageM50)

#Okay, they look different. Still not a great lifespan... 

##### BUT ARE THEY SIGNIFICANTLY DIFFERENT? #########

t.test(ageM50, ageF50)
#yes or no? 

#let's plot them: The following command tells the 
#plot window to make two on one page. 

par(mfrow = c(1, 2)) #as in 1 row, two columns

boxplot(ageF50)

boxplot(ageM50)


####CONGRATULATIONS! You made a boxplot! DO a five second happy dance!

#Now let's compare with histograms
#reload the original command to pull the numbers out of the CXV files

ageF50 <- sort(F50$A)
ageM50 <-sort(M50$A)

#and now we can make histograms

hist(ageM50)
hist(ageF50)

#ANd how about a little color? 

hist(ageF50, col="pink3")
hist(ageM50, col ="steelblue")

#the "col=" part of the command above is called an argument. 
#we can add more arguments (there are TONS for plotting graphs.)
#here are a few. Make sure you use quotes after the equal sign. 
#xlab= means label of x axis, ylab= x axis label
#main= the chart title, lwd= line width dimension

hist(ageF50, col="pink3", ylab="Count", 
     xlab="Age at Death", 
     main = "Female Lifespan, 1850 cohort")

hist(ageM50, col ="steelblue", ylab="Count", 
     xlab="Age at Death", 
     main = "Male Lifespan, 1850 cohort")

#now hit zoom on the plot window, and behold the magnificence.

############################# Section 2 ###############################

#Reload everything up to this point if you are starting over: 
#remember, arguments can be used in all types of situations. 
#the "col=" part of the command above is called an argument. 
#we can add more arguments (there are TONS for plotting graphs.)
#here are a few. Make sure you use quotes after the equal sign. 
#xlab= means label of x axis, ylab= x axis label
#main= the chart title, lwd= line width dimension

par(mfrow = c(1, 2)) #as in 1 row, two columns, putting 2 graphs side by side

hist(ageF50, col="pink3", ylab="Count", 
     xlab="Age at Death", 
     main = "Female Lifespan, 1850 cohort")

hist(ageM50, col ="steelblue", ylab="Count", 
     xlab="Age at Death", 
     main = "Male Lifespan, 1850 cohort")

#now hit zoom on the plot window, and behold the magnificence.

#the dev.off() command erases all settings in the plot window and resets it. 
#so before making any new plots, it is a good idea to clear the memory
#like hitting C a bunch of times on a calculator. 

#dev.off()

par(mfrow=c(1, 2))
boxplot(ageF50, lwd=2, col="pink3",main="Female Lifespan", ylab="Age, in years")

boxplot(ageM50, col="steelblue", lwd=2, main="Male Lifespan", ylab="Age, in years")

#Create pink and blue boxplots and histograms for the 
#1850's and 1880's cohort, including y and x labels and titles. 
#You can use any other two colors if heteronormativity triggers you. 
#but it was just easy to make them that for this exercise.

#use the export function in the plot window to save the graphics 
#as renamed, descriptive .PNG files in a folder that you are using. 

#1. what are some features that you can interpret in the 1880's M and F histograms? 

#2. Compare the Male 1850's and 1880's boxplots. How do they differ? 

#3. are the life expectancies of M1850 and M1880 Significantly different? 

#4. Are the life expectancies of F1850 and F1880 significantly different? 

#5. What is the average lifespan of males and females in 1880?

#6. do the comparisons you are making between M/F in 1850 and 1880 change in outcome if you create boxplots and 
#histograms for the entire population in those cohorts? Are those differences significant? 

###################### Section 3 #############################

dev.off()

# It is assumed that life expectance has gone up through time, but this is not always true. 

#in this exercise, we will see if all of our cemetery data make sense with this generalization. 

#Let's grab the entire data set. R can also import excel files, but sometimes is a little sloppy 
#and grabs parts of the file you don't want. We can subset later to grab exactly what we want.

#there is a library that is designed to translate Excel files to r documents, called "readxl". 
#You will need to install this. 

#at the top of the R-studio menu, choose Tools-->install packages-->and 
#in the "packages" window, type readxl. this 
#downloads the library and puts it on your machine. 
#next, you have to load it. this uses the library(package name here) format. 
#When I write scripts, I like to put all the libraries at the 
#very top of my workspace script so all the tools 
#I need for the whole analysis are loaded at the same time. 

library(readxl)

# Congrats, you just installed your very first package! Do a 5 second happy dance. 

#now in Rstudio, go to "global environment" window --> import dataset -->from excel.
#choose cemdata.xlsx, -->import
##or take the pound signs out of this command: 
#cemdata <- read_excel("cemdata.xlsx")

#you will notice that the cohorts that we used for counting in the lab are still there. 
#Lets grab them using a subset function and save them for later. 

cohort <- (cemdata[ 1:21, 7:8]) #the object cohort is made up of the cemdata sheet, 
#rows 1 to 21, and columns 7 to 8. It is the counts of how many people were born in which year.)..
plot(cohort)
library(ggplot2)

###to do dual y-axis you have to scale the data first then reverse the scale on the sec_axis
cohort_year <- mutate(cohort, year=cohort*10)
USA_POP<- mutate(USA_POP,"popperscaled"=pop/40000)


ggplot(cohort_year, aes(x=year, y=count))+
        geom_point()+
        geom_line(data=USA_POP, aes(x=Year, y=popperscaled))+
        scale_y_continuous(sec.axis = sec_axis(~ . * 40000, name = "USA POP scaled by 1:40000"))




#You can plot the cohort counts with "plot(cohort)"
#now, lets subset the cemdata. 
wholecem<-(cemdata[1:9442, 1:5])

View(wholecem)

plot(wholecem$`Age at Death`~wholecem$Cohort) #this gives us kind of an ugly plot, but clearly shows 
#where we have a bunch of data. The tilde (~) means against... So that line of code says 
#Plot the age at death against the cohorts".

#Here, a boxplot function would give a nicer summary: it would tell us the 1st and 3rd quartiles, median, and outliers.
#and boxplots are even better if they are pretty:

boxplot(wholecem$`Age at Death`~wholecem$Cohort)

#install package "RColorBrewer" and load it.

library(RColorBrewer)

#RColorBrewer is kind of a weird package, since it makes up colors and then uses a command called 
#brewer.pal to talk back to the r script. When you have multiple things going on, the color brewer
#gives you as many colors as you need, so you don't have to code each choice. (which is a HUGE PITA, BTW)

boxplot(wholecem$`Age at Death`~wholecem$Cohort, col=brewer.pal(7,"BrBG"))

#google brewer pal palettes and make the color palette as you like. The .pdf from NCEAS is pretty good. 

#now copy that command line from line #245 down below here, add x and y labels, a title, and make it a different
#set of colors. Also, make the lines for the outliers twice as thick as they are now. Export this image as a 
#.JPEG file and save to your folder. 




# 7. can this beautiful work of mathematical art be used to make the argument that life expectancy went up for the years
#1730 to 1910? What do you see? 
#8. What problems are there with this data set? Use ( plot(wholecem$`Age at Death`~wholecem$Cohort))
#to answer this, and include it near your answer.
#9. in Ms Word, import all ten graphics. make sure that they are all resized to approximately 2" tall and have word wrapping around them. #They will all contain x and y labels, titles, and be in color. They will each have captions (Fig 1: blah de blah’s). 
#10. Create a table in your word document that contains the t-Test information: relationship being tested, 
#(F/M, 1850, F/M 1880, whole 1850 to 1880, M/M 50/80, and F/F 50/80). Include columns for t scores, degrees of freedom, p values, and significance. To demonstrate significance, use the industry standard of –=<0.05, *=<0.01, **=<0.001, and ***=<0.0001. 
#11. Interpret the significance chart. What does this small spread of numbers tell you? How do you explain it to someone? 


