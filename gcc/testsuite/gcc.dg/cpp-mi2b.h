/* Test for overly eager multiple include optimization.
   Problem distilled from glibc 2.0.7's time.h, sys/time.h, timebits.h. 
   Problem noted by Tom Tromey <tromey@cygnus.com>.  */

#define need_x
#include "cpp-mi2c.h"
