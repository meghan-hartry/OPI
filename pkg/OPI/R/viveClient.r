#
# OPI for HTC VIVE Pro Eye
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: Mar 2019 
#
# Copyright 2019 Andrew Turpin
#
# This program is part of the OPI (http://perimetry.org/OPI).
# OPI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Modified
#
# To Do:
# Add a look up table to convert the opacity level to a cd/m^2 value
# Add a look up table to convert the coordinate plane to degrees of visual angle

###################################################################
# .OpiEnv$Vive$socket is the connection to the Vive
# .OpiEnv$Vive$...    constants and setting variables
###################################################################
if (exists(".OpiEnv") && !exists("Vive", where=.OpiEnv)) {
  assign("Vive", new.env(25), envir=.OpiEnv)
  
  .OpiEnv$Vive$socket <- NA
  
  .OpiEnv$Vive$SEEN     <- 1  
  .OpiEnv$Vive$NOT_SEEN <- 0  
}

#' @rdname opiInitialize
#' @details
#' \subsection{Vive}{
#'   \code{opiInitialize(ip="127.0.0.1", port=50008)}
#'   
#'   If the chosen OPI implementation is \code{Vive}, then you may specify
#'   the IP address and port of the socket server for the Vive.
#'   
#'   \itemize{
#'     \item\code{ip} is the IP address of the Vive server as a string
#'     \item\code{port} is the TCP/IP port of the Vive server as a number
#'   }
#' }
#' @return
#' \subsection{Vive}{
#'   Always returns NULL.
#' }
vive.opiInitialize <- function(
  ip="127.0.0.1",
  port=50008
) {
  cat("Found server at",ip, "on port", port,":)\n")
  
  socket <- tryCatch(
    socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
    error=function(e) stop(paste("Cannot connect to server at", ip, "on port", port))
  )
  assign("socket", socket, envir = .OpiEnv$Vive)
  
  return(NULL)
}

###########################################################################
# INPUT: 
#   As per OPI spec. Note eye is part of stim object
#
# Return a list of 
#    err             : (integer) 0 all clear, >= 1 some error codes (eg cannot track, etc)
#    seen            : 0 for not seen, 1 for seen (button pressed in response window)
#    time            : in ms (integer) (does this include/exclude the 200ms presentation time?) -1 for not seen.
###########################################################################
#' @rdname opiPresent
#' @details
#' \subsection{Vive}{
#'   \code{
#'   stim <- list(x=2, y=2, level=100, duration=3500, responseWindow=3500, size=1, eye="left", color="white")
#'   class(stim) <- "opiStaticStimulus"
#'   opiPresent(stim)}
#'   \itemize{
#'     \item{\code{stim}} the stimulus object to present
#'     \item{\code{stim$x}} fixation x coordinate, such as \code{1.0}.
#'     \item{\code{stim$y}} fixation y coordinate, such as \code{1.0}.
#'     \item{\code{stim$level}} stimulus level, as a percentage of opacity, between \code{0.00} and \code{100.00}.
#'     \item{\code{stim$duration}} stimulus duration in milliseconds, such as \code{2000}.
#'     \item{\code{stim$responseWindow}, stimulus response window in milliseconds, such as \code{3500}.
#'     \item{\code{stim$size} stimulus size, as a scalar multiplier, \code{1.0} is default.
#'     \item{\code{stim$eye}} eye to present to, can be \code{'left'}, \code{'right'}, or \code{'both'} (default).
#'     \item{\code{stim$color}} color of the stimulus, can be \code{'white'} (default), \code{'black'}, \code{'yellow'}, \code{'clear'}, \code{'grey'}, \code{'gray'}, 
#'       \code{'magenta'}, \code{'cyan'}, \code{'red'}, \code{'blue'}, or \code{'green'}.
#'     \item{\code{stim$rgb}} color of the stimulus as string of RGB values separated by spaces, such as \code{'255 255 255'}, optional, overrides \code{stim$color}.
#'   }
#'   
#'   If the chosen OPI implementation is \code{Vive}, then \code{nextStim}
#'   is ignored.
#'   Currently only implemented for \code{opiStaticStimulus}.
#' }
vive.opiPresent <- function(stim, nextStim=NULL) { UseMethod("vive.opiPresent") }
setGeneric("vive.opiPresent")

vive.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
  if (is.null(stim)) return(list(err = "no stimulus"))
  if (is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
  if (is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
  if (is.null(stim$level)) return(list(err="No level in stimulus", seen=NA, time=NA))
  if (is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
  if (is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))

  # if no info about size, then it is 1
  if(is.null(stim$size)) stim$size <- 1
  
  # if no info about eye, then it is both  
  if(is.null(stim$eye)) stim$eye <- "both"
  
  # if no info about color, then it is white
  if(is.null(stim$color)) stim$color <- "white"
  
  # if no info about stimulus RGB, then it is null
  if(is.null(stim$rgb)) stim$rgb <- "null null null"
  
  # send message to present the stimulus
  msg <- sprintf("OPI_PRESENT %s %0.1f %0.1f %0.2f %0.1f %0.0f %0.0f %s %s", stim$eye, stim$x, stim$y, stim$level, stim$size, stim$duration, stim$responseWindow, stim$color, stim$rgb)
  writeLines(msg, .OpiEnv$Vive$socket)
  
  # record results
  seenBool <- readBin(.OpiEnv$Vive$socket, "logical", size=1)
  
  # if no info returned, error
  if(is.null(seenBool)){
    return(list(
      err  = "Unknown error",
      seen = 0, 
      time = 0
    ))
  }
  
  if(seenBool){
    seen<-1
  }else{
    seen<-0
  }
  
  time <- readBin(.OpiEnv$Vive$socket, "integer", size=4)

  return(list(
    err  = NULL,
    seen = seen,    # assumes 1 or 0, not "true" or "false"
    time = time
  ))
}

########################################## 
# Present kinetic stim, return values 
########################################## 
vive.opiPresent.opiKineticStimulus <- function(stim, ...) {
  warning("Vive does not support kinetic stimuli (yet)")
  return(list(err="Vive does not support kinetic stimuli (yet)", seen=FALSE, time=0))
}

###########################################################################
# Not supported on Vive
###########################################################################
vive.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
  warning("Vive does not support temporal stimuli (yet)")
  return(list(err="Vive does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

#' @rdname opiSetBackground
#' @param fix_cx fixation x coordinate position. Default is \code{0.0}.
#' @param fix_cy fixation y coordinate position. Default is \code{0.0}.
#' @param fix_sx fixation horizontal size scalar multiplier. Default is \code{1.0}.
#' @param fix_sy fixation vertical size scalar multiplier. Default is \code{1.0}.
#' @param fix_color fixation color. Default is \code{'white'}.
#' @param eye sets the active background configuration. Default is \code{'left'}.
#' @details
#' \subsection{Vive}{
#'   \code{opiSetBackground(lum=100, color="white", fixation="Cross", fix_cx=0, fix_cy=0, fix_sx=1, fix_sy=1, fix_lum=100, fix_color="white", eye="null")}
#'   \itemize{
#'     \item{\code{lum}} background opacity, as a percentage, between \code{0.00} and \code{100.00} (default).
#'     \item{\code{color}} color of the background, can be \code{'black'} (default), \code{'white'}, \code{'yellow'}, \code{'clear'}, \code{'grey'}, \code{'gray'}, 
#'       \code{'magenta'}, \code{'cyan'}, \code{'red'}, \code{'blue'}, or \code{'green'}.
#'     \item{\code{rgb}} color of the background as string of RGB values separated by spaces, such as \code{'255 255 255'}, optional, overrides \code{color}.
#'     \item{\code{fixation}} can only be \code{'Cross'} at the moment.
#'     \item{\code{fix_cx}, \code{fix_cy}} fixation (x, y) coordinate positions
#'     \item{\code{fix_sx}, \code{fix_sy}} dimensions of fixation target as scalar multipliers
#'     \item{\code{fix_lum}} CURRENTLY IGNORED. fixation opacity, as a percentage, between \code{0.00} and \code{100.00} (default). 
#'     \item{\code{fix_color}} color of the fixation target, can be \code{'white'} (default), \code{'black'}, \code{'yellow'}, \code{'clear'}, \code{'grey'}, \code{'gray'}, 
#'       \code{'magenta'}, \code{'cyan'}, \code{'red'}, \code{'blue'}, or \code{'green'}.
#'     \item{\code{fix_rgb}} color of the fixation target as string of RGB values separated by spaces, such as \code{'255 255 255'}, optional, overrides \code{fix_color}.
#'     \item{\code{eye}} Sets the background configuration, can be \code{'left'}, \code{'right'}, or \code{'both'} (default).
#'   }
#' }
#' @return
#' \subsection{Vive}{ 
#'   DETAILS
#' }
vive.opiSetBackground <- function(lum=100, color="black", rgb="NULL NULL NULL", fixation="Cross", 
                                  fix_cx=0.0, fix_cy=0.0, fix_sx=1.0, fix_sy=1.0, fix_lum=100.00,
                                  fix_color="white", fix_rgb="NULL NULL NULL", eye="both") {
  msg <- sprintf("OPI_SET_BGROUND %s %0.1f %0.1f %0.1f %0.1f %0.2f %s %s %0.2f %s %s %s", fixation, fix_cx, fix_cy, fix_sx, fix_sy, fix_lum, fix_color, fix_rgb, lum, color, rgb, eye)
  writeLines(msg, .OpiEnv$Vive$socket)
  
  res <- readBin(.OpiEnv$Vive$socket, "logical", size=1)
  if (res != TRUE)
    return("Trouble changing background in opiSetBackground")
  return(NULL)
}

##############################################################################
#### return list(err=NULL, fixations=matrix of fixations)
####       matrix has one row per fixation
####       col-1 timestamp (ms since epoch) 
####       col-2 x in degrees 
####       col-3 y in degrees 
##############################################################################
#' @rdname opiClose
#' @return
#' \subsection{Vive}{
#'   DETAILS
#' }
vive.opiClose <- function() {
  writeLines("OPI_CLOSE", .OpiEnv$Vive$socket)
  
  res <- readLines(.OpiEnv$Vive$socket, n=1)
  
  close(.OpiEnv$Vive$socket)
  
  if (!res)
    return(list(err="Trouble closing Vive connection."))
  else
    return(NULL)
}

##############################################################################
#### Test Connection
##############################################################################
#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @details
#' \subsection{Vive}{
#'   Returns whether the socket port has a valid connection.
#' }
vive.opiQueryDevice <- function() {
  msg <- paste("OPI_QUERY_DEVICE")
  writeLines(msg, .OpiEnv$Vive$socket)
  res <- readBin(.OpiEnv$Vive$socket, "logical", size=1)
  return(res)
}