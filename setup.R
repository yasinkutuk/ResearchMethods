
plot.norm <- function(mu, sd, xlab.name="Variable",
                      new=TRUE,
                      shade.lo.x=NA, shade.hi.x=NA,
                      shade.lo.z=NA, shade.hi.z=NA,
                      show.lo=NA, show.hi=NA,
                      round.dec=1,
                      shade.col="wheat",
                      main="",
                      width=6, # WAS 3.5
                      height=width,
                      type="z",
                      las=1,
                      xlim.hi = NA, xlim.lo = NA,
                      zlim.hi = 3.5, zlim.lo=-zlim.hi){
  
  # mu  is the mean of the distn
  # sd  is the std dev of the distn
  # xlab.name  is the  xlab  label
  # new is TRUE by default: a new plot is drawn.  If FALSE, the plot is added to the current device 
  # shade.lo.x  is the lower shade limit (in terms of x, not z)
  # shade.hi.x  is the upper shade limit (in terms of x, not z)
  # shade.lo.z  is the lower shade limit (in terms of z, not x)
  # shade.hi.z  is the upper shade limit (in terms of z, not x)
  # show.lo  is a LOGICAL for showing the lower x-score 
  #   If it is a number, that number is placed at the lo position instead
  # show.hi  is a LOGICAL for showing the lower x-score explicitly
  #   If it is a number, that number is placed at the lo position instead
  # zlim.lo  /zlim.hi  is the lower (upper) limit of z on which to draw
  # round.dec  is the number of decimals to round to on the shown x-axis
  #    (full precision used in calculations)
  # type  is the the type of course, generally "z" or "t", placed as a label on the horizontal axis
  # las: The  las  parameter in par, for labelling horizontal axis
  # shade.col  is the shading colour, defaulting to "wheat" (see ?colours)
  # main  is the main title to use
  # width  and  height  specify the width and height of the x11 device window
  
  if ( is.na(shade.lo.x) & is.na(shade.lo.z) ) {
    warning("One of  shade.lo.x  and shade.lo.z  must be given.")
  }
  if ( is.na(shade.hi.x) & is.na(shade.hi.z) ) {
    warning("One of  shade.hi.x  and shade.hi.z  must be given.")
  }
  
  if ( new ) {
    par(mar=c(2,0,2,0) + 0.1  )
  }
  
  if (!is.na(xlim.hi) ){
    zlim.hi <- (xlim.hi-mu)/sd
  }
  if (!is.na(xlim.lo) ){
    zlim.lo <- (xlim.lo-mu)/sd
  }
  
  hor <- seq(zlim.lo, zlim.hi, length=250) # z-scores
  nc <- dnorm(hor, 0, 1) # Normal curve
  extra <- 0.25 # extra space at ends
  spacer <- -0.05 # space to other x-axis
  text.loc.z <- c(-3, -2, -1, 0, 1, 2, 3) 		# Where to place x-axis labels: In terms of z
  text.loc.x <- round(mu + text.loc.z * sd, round.dec) 		# Where to place x-axis labels: In terms of x 
  
  if ( is.na(shade.lo.z) ) {
    shade.lo.z <- (shade.lo.x - mu)/sd
  }
  if ( is.na(shade.hi.z) ) {
    shade.hi.z <- (shade.hi.x - mu)/sd
  }
  if (is.na(shade.lo.x) ) {
    shade.lo.x <- shade.lo.z * sd + mu
  }
  if (is.na(shade.hi.x) ) {
    shade.hi.x <- shade.hi.z * sd + mu
  }
  
  if (new) {
    plot( nc ~ hor, 
          axes=FALSE,
          ylim=c(-0.1, 0.4),
          xlim=c(zlim.lo-2*extra , zlim.hi+2*extra),
          lwd=2,
          xlab="",
          ylab="",
          main=main,
          type="l")
  }
  
  # Horizontal axis
  lines( c(zlim.lo-extra, zlim.hi+extra), 
         c(0,0), 
         lwd=2 )
  
  # Add arrow to axis
  arrows(0, 0, 3.75, 0, 
         length=0.15, 
         angle=20, 
         lwd=2)
  
  # Add line corresponding to the mean (z=0)
  lines( c(0,0.4) ~ c(0, 0), 
         lwd=2,
         col="grey")
  
  # Titles
  title(sub=xlab.name, 
        line=0)
  
  # Label axis;  type  is usually "t" or "z"
  text(zlim.hi+extra, 0, pos=3, adj=0, type)
  
  # Label horizontal axis
  text( text.loc.z, 0, 
        pos=1, 
        las=las,
        labels=as.character(text.loc.x) )
  
  
  # Add lines to demarcate shading
  lines(c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)), lwd=2)
  lines(c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)), lwd=2)
  
  # What to shade?
  if ( !is.na(show.lo) ) {
    if (is.logical(show.lo) ){
      if ( show.lo ) {
        lines( c(shade.lo.z, shade.lo.z), c(spacer, dnorm(shade.lo.x,0,1)))
        text(shade.lo.z, -0.11, as.character(shade.lo.x) )
      }
    } else { # Not logical:  a value/character is supplied
      lines( c(shade.lo.z, shade.lo.z), c(spacer, dnorm(shade.lo.x,0,1)))
      text(shade.lo.z, -0.11, as.character(show.lo) )
    }
  }
  
  if ( !is.na(show.hi) ) {
    if (is.logical(show.hi) ){
      if( show.hi ) {
        lines( c(shade.hi.z, shade.hi.z), c(spacer, dnorm(shade.hi.x,0,1)))
        text(shade.hi.z, -0.11, as.character(shade.hi.x) )
      }
    } else {
      lines( c(shade.hi.z, shade.hi.z), c(spacer, dnorm(shade.hi.x,0,1)))
      text(shade.hi.z, -0.11, as.character(show.hi) )
    }
  }
  
  # Add Shading
  x.poly <- seq( max(zlim.lo, shade.lo.z), 
                 min(zlim.hi, shade.hi.z), length=100)
  y.poly <- dnorm( x.poly, 0, 1)
  
  x.p <- c( x.poly, rev(x.poly) )
  y.p <- c( rep(0, length(x.poly)), rev(y.poly) )
  polygon(x.p, y.p, col=shade.col )
  
  # Rug on horizontal axis
  yrange <- max(nc) - 0
  for (i in 1:length(text.loc.z)) {
    lines( c(text.loc.z[i], text.loc.z[i]),
           c(0, 0.05*yrange),
           col="grey")
  } 
}





##############################################################
##############################################################
##############################################################
##############################################################






plot.normZ <- function(mu, sd, xlab.name="Variable",
                       new=TRUE,
                       shade.lo.x=NA, shade.hi.x=NA,
                       shade.lo.z=NA, shade.hi.z=NA,
                       show.lo=NA, show.hi=NA,
                       round.dec=1,
                       shade.col="wheat",
                       main="",
                       width=6, # WAS 3.5
                       height=width,
                       type="z",
                       las=1,
                       zlim.hi = 3.5, zlim.lo=-zlim.hi){
  
  # mu  is the mean of the distn
  # sd  is the std dev of the distn
  # xlab.name  is the  xlab  label
  # new is TRUE by default: a new plot is drawn.  If FALSE, the plot is added to the current device 
  # shade.lo.z  is the lower shade limit (in terms of z, not x)
  # shade.hi.z  is the upper shade limit (in terms of z, not x)
  # show.lo  is a LOGICAL for showing the lower x-score 
  #   If it is a number, that number is placed at the lo position instead
  # show.hi  is a LOGICAL for showing the lower x-score explicitly
  #   If it is a number, that number is placed at the lo position instead
  # zlim.lo  /zlim.hi  is the lower (upper) limit of z on which to draw
  # round.dec  is the number of decimals to round to on the shown x-axis
  #    (full precision used in calculations)
  # type  is the the type of course, generally "z" or "t", placed as a label on the horizontal axis
  # las: The  las  parameter in par, for labelling horizontal axis
  # shade.col  is the shading colour, defaulting to "wheat" (see ?colours)
  # main  is the main title to use
  # width  and  height  specify the width and height of the x11 device window
  
  #if ( is.na(shade.lo.z) ) {
  #	warning(" shade.lo.z  must be given.")
  #}
  #if (  is.na(shade.hi.z) ) {
  #	warning(" shade.hi.z  must be given.")
  #}
  
  if ( new ) {
    #   quartz(width=width,height=height, bg="white")
    par(mar=c(2,0,2,0) + 0.1  )
  }
  
  if ( is.na(shade.lo.z) ) {
    shade.lo.z <- (shade.lo.x - mu)/sd
  }
  if ( is.na(shade.hi.z) ) {
    shade.hi.z <- (shade.hi.x - mu)/sd
  }
  if (is.na(shade.lo.x) ) {
    shade.lo.x <- shade.lo.z * sd + mu
  }
  if (is.na(shade.hi.x) ) {
    shade.hi.x <- shade.hi.z * sd + mu
  }
  
  
  hor <- seq(zlim.lo, zlim.hi, length=250) # z-scores
  nc <- dnorm(hor, 0, 1) # Normal curve
  extra <- 0.25 # extra space at ends
  spacer <- -0.05 # space to other x-axis
  text.loc <- c(-3, -2, -1, 0, 1, 2, 3) # In terms of z
  
  
  if (new) {
    plot( nc ~ hor, 
          axes=FALSE,
          ylim=c(-0.1, 0.4),
          xlim=c(zlim.lo-2*extra , zlim.hi+2*extra),
          lwd=2,
          xlab="",
          ylab="",
          main=main,
          type="l")
  }
  
  lines( c(zlim.lo-extra, zlim.hi+extra) , c(0,0), lwd=2 )
  #arrows(0, spacer, 3.75, spacer, length=0.15, angle=20, lwd=2)
  
  #lines( c(zlim.lo-extra, zlim.hi+extra) , c(spacer, spacer), lwd=2 )
  #arrows(0, 0, 3.75, 0, length=0.15, angle=20, lwd=2)
  
  lines( c(0,0.4) ~ c(0, 0), lwd=2 )
  
  # text(zlim.hi+1.5*extra, spacer, adj=0,"z")
  #text(zlim.hi-extra, 2*spacer, pos=1, adj=0,xlab.name)
  title(sub=xlab.name, line=0)
  
  #text(zlim.hi+1.5*extra, 0, adj=0,xlab.name)
  text(zlim.hi+extra, 0, pos=3, adj=0, "z")
  
  text( text.loc, 0, pos=1, labels=as.character(text.loc) )
  #text( text.loc, spacer, pos=1, 
  #   labels=as.character(round(text.loc*sd + mu, round.dec)) )
  
  
  lines(c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)), lwd=2)
  lines(c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)), lwd=2)
  
  if ( !is.na(show.lo) ) {
    if (is.logical(show.lo) ){
      if ( show.lo ) {
        lines( c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)))
        text(shade.lo, -0.11, as.character(shade.lo.z) )
        #	      text(shade.lo, spacer-150, as.character(shade.lo.z) )
      }
    } else { # Not logical:  a value/character is supplied
      lines( c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)))
      #      text(shade.lo, spacer-0.05, as.character(show.lo) )
      #      text(shade.lo, -0.11, as.character(show.lo) )
    }
  }
  
  if ( !is.na(show.hi) ) {
    if (is.logical(show.hi) ){
      if( show.hi ) {
        lines( c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)))
        #text(shade.hi.z, -0.11, as.character(shade.hi.z) )
      }
    } else {
      lines( c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)))
      #text(shade.hi.z, -0.11, as.character(show.hi) )
    }
  }
  
  # Shading
  x.poly <- seq( max(zlim.lo, shade.lo.z), 
                 min(zlim.hi, shade.hi.z), length=100)
  y.poly <- dnorm( x.poly, 0, 1)
  
  x.p <- c( x.poly, rev(x.poly) )
  y.p <- c( rep(0, length(x.poly)), rev(y.poly) )
  polygon(x.p, y.p, col=shade.col )
  
  # Rug on horizontal axis
  yrange <- max(nc) - 0
  for (i in 1:length(text.loc)) {
    lines( c(text.loc[i], text.loc[i]),
           c(0, 0.05*yrange),
           col="grey")
  } 
}







# Define the six steps to use repeatedly
SixSteps <- function( Flag=NA, Text=NA, col.Flag="slategray4", col.Default="slategray1", Labs="Long", Arrows=TRUE, ...){
  
  # To use  Text  in expressions, we need to replace the spaces with tildes
  library(stringr)
  if (!is.na(Text)) Text <- str_replace_all(Text," ", "~")
  
  if (is.na(Flag) ) Flag <- -1
  
  BoxWidth<- 2
  BoxHeight <- 0.4
  BoxGap <- 0.4
  
  Labels.Long <- c(
    "Ask~the~question",
    "Design~the~study",
    "Collect~the~data",
    "Describe,~summarise~the~data",
    "Analyse~the~data",
    "Report~the~results")
  
  Labels.Short <- c("Ask",
                    "Design",
                    "Collect",
                    "Describe",
                    "Analyse",
                    "Report")
  
  if(Labs=="Short") Labels <- Labels.Short
  if(Labs=="Long") Labels <-  Labels.Long
  if(Labs=="None") Labels <- rep("", 6)
  if(Labs=="Random") Labels <- Labels.Long[ sample(1:length(Labels.Long))]
  
  # Some fooling about to make the main step in bold, and any substeps in plain.
  if ( !(Labs=="None")){ # So if we are adding labels...
    for (j in (1:6)){    # Treat each one at a time..
      if ( j == Flag) {  # And if this is the ne we want flagged
        if (is.na(Text)) { #*and* there no text to add...
          Labels[j] <- paste("expression(bold(", Labels[j], "))", sep="")
        } else{            # *and* there is  text to add...
          Labels[j] <- paste("expression(bold(", Labels[j], "):", Text, ")", sep="")
        }
      } else {
        Labels[j] <- paste("expression(bold(", Labels[j], "))", sep="")
        
      }
    }
  }
  
  par( mar=c(0, 0, 0, 0) + 0.1)
  plot( c(-BoxWidth/2, BoxWidth/2),
        c(-BoxHeight/2, 5.5*(BoxHeight + BoxGap)),
        type="n",
        xlab="",
        ylab=",",
        axes=FALSE)
  
  DrawBox <- function(which.box, flag=FALSE, Locations=Locations){
    Loc <- (which.box - 1) * (BoxHeight+BoxGap)
    polygon( c( -BoxWidth/2, -BoxWidth/2, BoxWidth/2, BoxWidth/2),
             c(Loc - BoxHeight/2, Loc + BoxHeight/2, Loc + BoxHeight/2, Loc - BoxHeight/2),
             col=ifelse(flag, col.Flag, col.Default),
             border=NA)
  }
  
  DrawLine <- function(from, to, BoxHt=BoxHeight, BoxGp=BoxGap){
    
    to.y     <- BoxHt*(6 - to)   + BoxGp*(6 - to     ) + BoxHt/2
    from.y   <- BoxHt*(6 - from) + BoxGp*(6 - from   ) - BoxHt/2 # from.y - BoxHeight - BoxGap
    
    
    arrows(0, from.y,
           0, to.y,
           length=0.15,
           lwd=2,
           angle=15)  
  }
  
  for (i in (1:6)){
    if ( (7-i) == Flag){
      DrawBox(i, TRUE)
    } else{
      DrawBox(i, FALSE)
    }
    if (Arrows) if (i > 1 ) DrawLine(i-1, i)
  }
  
  # Add text into boxes
  for (i in (1:length(Labels))){
    Loc <- (i - 1) * (BoxHeight+BoxGap)
    
    text.string <- paste("text(0, Loc, ", rev(Labels)[i], ")")
    eval( parse(text=text.string))
    #    text(0, Loc, rev(Labels)[i])
  }
  
}
