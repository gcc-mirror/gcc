/* Test for overly eager multiple include optimization.
   Problem distilled from glibc 2.0.7's time.h, sys/time.h, timebits.h. 
   Problem noted by Tom Tromey <tromey@cygnus.com>.  */

#include "mi2c.h"
