##Following libraries need to be installed for running the program
##by using the install.packages(c("raster","rgdal","ncdf","gdata"))
library(raster)
library(ncdf)
library(rgdal)
library(gdata)
#Using setwd to direct the unzipped .bil files like
setwd("D:/Weather/PRISM_ppt_stable_4kmD1_20130101_20131231_bil")
#******************************************************************#
#***function for merge daily data *********************************#
#***Parameter: variable name      *********************************#
#******************************************************************#
MergeBIL2NC<- function(variable,year)
{
  pattern <- paste("PRISM_",variable,"_stable_4kmD1_",year,".*.bil$",sep='')
  cat(paste("pattern, ", pattern,"!\n",sep="",collapse=""))
  files <- list.files(path=".",pattern=pattern,recursive=TRUE,full.names=TRUE) 
  cat(paste("filelist, ", files,"!\n",sep="",collapse=""))
  t <- stack(files)
  filename <- paste(variable,"_",year,".nc",sep='')
  cat(paste("filename, ", filename,"!\n",sep="",collapse=""))
  t_nc <- writeRaster(t,filename=filename,bandorder='BIL',overwrite=TRUE,format="CDF",varname=variable)
}
MergeBIL2NC("ppt",2013)
MergeBIL2NC("tmax",2013)
MergeBIL2NC("tmin",2013)

#Following script for extracting the weather information from the NetCDF file 
#put all the nc file(for  each year) into one directtory 
#******************************************************************#
#***function for extracting the weather for each state************#
#***Parameter: state name (there are point shapefile for each state)*******#
#******************************************************************#
weather4Point <- function(state)  
{
  #get the point file for the state
  setwd("/data/ecr/yangping/PRISM/State/")
  pl <- readOGR(".",paste("points4_",state,sep=''))
  setwd("/data/ecr/yangping/PRISM/NetCDF/")
  year <- 1981
  tminfile <- paste("tmin","_",year,".nc",sep='')
  b_tmin <- brick(tminfile,varname='tmin')
  pptfile <- paste("ppt","_",year,".nc",sep='')
  b_ppt <- brick(pptfile,varname='ppt')
  tmaxfile <- paste("tmax","_",year,".nc",sep='')
  b_tmax <- brick(tmaxfile,varname='tmax')
  #Get the first year here!!!
  print(paste("processing year :",year,sep=''))
  print(paste("processing state :", state,sep=''))
  for(l in 1:length(pl))
  {
    v <- NULL
    #generate file with the name convention with t_n(latitude)w(longitude).txt, 5 digits after point should be work
    filename <- paste("/data/ecr/yangping/PRISM/US1/N",round(coordinates(pl[l,])[2],5),"W",abs(round(coordinates(pl[l,])[1],5)),".wth",sep='')
    print(paste("processing file :",filename,"  for the state of :",state," for year : ",year,sep=''))
    tmin <- as.numeric(round(extract(b_tmin,coordinates(pl[l,])),digits=1))
    tmax <- as.numeric(round(extract(b_tmax,coordinates(pl[l,])),digits=1))
    ppt <- as.numeric(round(extract(b_ppt,coordinates(pl[l,])),digits=2))
    v <- cbind(tmax,tmin,ppt)
    tablename <- c("tmin","tmax","ppt")
    v <- data.frame(v)
    colnames(v) <- tablename
    v["default"] <- 0
    v["year"] <- year
    date <- seq(as.Date(paste(year,"/1/1",sep='')),as.Date(paste(year,"/12/31",sep='')),"days")
    month <- as.numeric(substr(date,6,7))
    day   <- as.numeric(substr(date,9,10))
    v["month"] <- month
    v["day"]  <-  day
    v <- v[c("year","month","day","default","tmin","tmax","ppt")]
    write.fwf(x=v,filename,append=FALSE,na="NA",rownames=FALSE,colnames=FALSE,width=c(6,3,3,5,5,5,6))
  }
  for (year in 1982:2013)
  {
    #get the combined netCDF file
    tminfile <- paste("tmin","_",year,".nc",sep='')
    b_tmin <- brick(tminfile,varname='tmin')
    pptfile <- paste("ppt","_",year,".nc",sep='')
    b_ppt <- brick(pptfile,varname='ppt')
    tmaxfile <- paste("tmax","_",year,".nc",sep='')
    b_tmax <- brick(tmaxfile,varname='tmax')
    #Get the first year here!!!
    print(paste("processing year :",year,sep=''))
    for(l in 1:length(pl))
    {
      v <- NULL
      #generate file with the name convention with t_n(latitude)w(longitude).txt, 5 digits after point should be work
      filename <- paste("/data/ecr/yangping/PRISM/US1/N",round(coordinates(pl[l,])[2],5),"W",abs(round(coordinates(pl[l,])[1],5)),".wth",sep='')  
      print(paste("processing file :",filename,"  for the state of :",state," for year : ",year,sep=''))
      tmin <- as.numeric(round(extract(b_tmin,coordinates(pl[l,])),digits=1))
      tmax <- as.numeric(round(extract(b_tmax,coordinates(pl[l,])),digits=1))
      ppt <- as.numeric(round(extract(b_ppt,coordinates(pl[l,])),digits=2))
      v <- cbind(tmax,tmin,ppt)
      tablename <- c("tmin","tmax","ppt")
      v <- data.frame(v)   
      colnames(v) <- tablename
      v["default"] <- 0
      v["year"] <- year
      date <- seq(as.Date(paste(year,"/1/1",sep='')),as.Date(paste(year,"/12/31",sep='')),"days")
      month <- as.numeric(substr(date,6,7))
      day   <- as.numeric(substr(date,9,10))
      v["month"] <- month 
      v["day"]  <-  day
      v <- v[c("year","month","day","default","tmin","tmax","ppt")]
      v     
      #print(paste(v), zero.print = ".")
      #write into a file with the APEX format
      write.fwf(x=v,filename,append=TRUE,na="NA",rownames=FALSE,colnames=FALSE,width=c(6,3,3,5,5,5,6))
    }
  }
}

States <- c("AL","AZ","AR","CO","CT","DC","FL","GA","ID","IA","IN","KS","KY","LA","MA","ME","MI","MN","MT","NC","ND","NE","NH","NJ","NM","NV","OK","RI","SC","SD","TN","TX","UT","VT","WA","WI","WY","IL")
for (state in States)
{
  weather4Point(state)
}

