############################   Credits   ##########################################
#Calculating canopy cover with thresholding method
#Created by: Richard M. Smith 2/25/2020

# CRS: +proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
############################    Notes     #########################################
#Haung method seems to work best for NIR images
#Use NIR method for pixel percentage




##########################  Set working directory    ############################
setwd("C:/Users/smit3581/Desktop/ArcPractice/Adaptive250AGL/V1/Selected")
dst="C:/Users/smit3581/Desktop/ArcPractice/Adaptive250AGL/V1/Selected"
#########################   Install packages    #################################
library(raster)
library(sp)
library(tidyverse)
library(imager)
library(biocLit)
library(wvtool)
library(autothresholdr)
library(EBImage)
library(ijtiff)  

#############################  Program  ############################################
vector1=(list.files(dst, pattern = ".TIF")) #creating a vector of the names of the images in file

dataframe2 <- data.frame()   #creating a blank dataframe to hold data

for(i in vector1) {
        
        
        img                 <-brick(i)
        plotRGB(img, r=1, g=2, b=3)
        
        a                   <-ijtiff::read_tif(i)
        #ijtiff::display(a[, , 1, 1])
        auto_thresh(a, "otsu")
        mask <- auto_thresh_mask(a, "Huang", ignore_na = T)
        ijtiff::display(mask[, , 1, 1])    #[y, x, channel, plane]          
        
        g                   <- as.vector(mask[, , 1, 1]) #convert array to vector
        plant               <- length(g[g==FALSE])      #count plant pixels
        soil                <- length(g[g==TRUE])        #count soil pixels
        allPixels           <- (plant + soil)            #count all pixels
        #CanopyCover_adj    <- plant/allPixels           #calculate canopy %
        #CanopyCover        <- CanopyCover_adj*100   #Adjust
        CanopyCover_adj     <- plant/allPixels           #calculate canopy %
        CanopyCover         <- (CanopyCover_adj)*100   #Adjust
        
        ijtiff::display(mask[, , 1, 1])
        
        print("% Canopy cover =")
        print(CanopyCover)
        
        dataframe1          <- data.frame(i, CanopyCover, allPixels, soil, plant)
        dataframe2          <- rbind(dataframe2, dataframe1)
        
        
        
}
#################   Write CSV to file      #########################
write.csv(dataframe2, file = "C:/Users/smit3581/Desktop/PHDResearch2020/PPAC_i7/ModelPP/UsingGLI/R1_CC/CC_PPAC_200717.csv",row.names=F)

##########################   View RGB image   ###########################
img <- brick("proxy_0153.TIF")
plotRGB(img, r=1, g=2, b=3)

####################  View threshold for specific images  #####################
#ones that work best ::::"internodes", "mean", "moments", "Otsu", "percentile"
a <- ijtiff::read_tif("proxy_0153.TIF")

auto_thresh(a, "IJDefault", ignore_na = T)

mask <- auto_thresh_mask(a, "IJDefault", ignore_na = T)


masked <- auto_thresh_apply_mask(a, "")


ijtiff::display(mask[, , 1, 1])    #[y, x, channel, plane]          

#######################   Test different thresholding methods   ##################

a_th <- auto_thresh(a, "Huang")
mask <- auto_thresh_mask(a, "Huang")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Huang2")
mask <- auto_thresh_mask(a, "Huang2")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Intermodes")
mask <- auto_thresh_mask(a, "Intermodes")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "IsoData")
mask <- auto_thresh_mask(a, "IsoData")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Li")
mask <- auto_thresh_mask(a, "Li")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "MaxEntropy")
mask <- auto_thresh_mask(a, "MaxEntropy")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "mean")
mask <- auto_thresh_mask(a, "mean")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "MinErrorI")
mask <- auto_thresh_mask(a, "MinErrorI")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Minimum")
mask <- auto_thresh_mask(a, "Minimum")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Moments")
mask <- auto_thresh_mask(a, "Moments")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Otsu")
mask <- auto_thresh_mask(a, "Otsu")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Percentile")
mask <- auto_thresh_mask(a, "Percentile")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "RenyiEntropy")
mask <- auto_thresh_mask(a, "RenyiEntropy")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Shanbhag")
mask <- auto_thresh_mask(a, "Shanbhag")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Triangle")
mask <- auto_thresh_mask(a, "Triangle")    
ijtiff::display(mask[, , 1, 1])

a_th <- auto_thresh(a, "Yen")
mask <- auto_thresh_mask(a, "Yen")    
ijtiff::display(mask[, , 1, 1])




library(classInt)
library(EBImage)
library(RCurl)
install.packages("RCurl")

setwd("G:/Flight Data/LowAlt2019/NEPAC/ExtrctByMsk/30AGL/190619/Merge30/GLI/SplitRaster_GLI")
dst="G:/Flight Data/LowAlt2019/NEPAC/ExtrctByMsk/30AGL/190619/Merge30/GLI/SplitRaster_GLI"



b <- ijtiff::read_tif("Proxy_017.TIF")

rast <- raster("Proxy_07.TIF")

display(b, method = "raster")   #display raster
display(b*7, method = "raster")  #display raster 4x brighter

a2 <- a*7                       #Make brighter image
display(a2, method="raster")

a2_blur <- gblur(a2, sigma = )
display(c2.b.blur, method = "raster"



countNuclei <- function(b){   
        # blur the image
        w = makeBrush(size = 11, shape = 'gaussian', sigma = 5)  # makes the blurring brush
        img_flo = filter2(b*2, w) # apply the blurring filter
        display(img_flo * 4, method = "raster") # display the blurred image - brighter for display only. 
