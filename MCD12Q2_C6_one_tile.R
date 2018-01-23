rm(list = ls())
# Loading required libraries 
library(sp)
library(raster)
library(colorRamps)

## Extract all tiles
path <- '/projectnb/modislc/users/dsm/eval_modis_lc_061917/MCD12I6/Dormancy/'
search_str <- paste('*2001',sep='')
files <- list.files(path=path,pattern=glob2rx(search_str),full.names=T,include.dirs=F)
tiles_c6 <- substr(files,78,83)
tiles <- tiles_c6

# One tile
phe_met <- c('Dormancy','EVI_Amplitude','EVI_Area','EVI_Minimum','Greenup','Maturity','MidGreendown','MidGreenup','NumCycles','Peak','QA_Detailed','QA_Overall','Senescence')
year <- seq(2001,2015)

j=14

col <- matlab.like2(365*10)

i=100

for(i in 1:315){
  for(vari in c(5,8,6,13,7,1)){  
    
    par(mfrow=c(3,5),mgp=c(2,1,0),oma=c(3,2,1,2),mar=c(3,4,2,3))  
    for(j in 1:15){
      path <- paste('/projectnb/modislc/users/dsm/eval_modis_lc_061917/MCD12I6/',phe_met[vari],'/',phe_met[vari],'_',tiles[i],'_',year[j],sep='')
      ncol <- 2400
      nrow <- 2400
      nbands <- 2
      cnt <- ncol*nrow*nbands
      data <- readBin(path,what="integer",n=cnt,size=2,endian="little")
      data[data>30000] <- NA
      data1 <- data - as.numeric(as.Date(paste(year[j]-1,'-12-31',sep='')))
      data2 <- array(data1,c(nbands, ncol, nrow))
      data2 <- aperm(data2, c(3,2,1)) #for transposing
      aa <- brick(data2)  
      plot(aa[[1]],bty = "n",xaxt = "n", yaxt = "n",colNA='grey45',col=col,main=paste('C6_',tiles[i],'_',year[j],'_',phe_met[vari],sep=''))
    }
    
    
    
    
      
    
  } 
  setwd(paste('/projectnb/modislc/users/mkmoon/LCD_C6/figure/raster/',year[j],'/',sep=''))
  dev.copy(png,paste('raster_C6_',tiles[i],'_',year[j],'.png',sep=''),width=1500,height=780)
  dev.off()
}

 
 


