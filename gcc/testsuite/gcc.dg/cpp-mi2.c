/* Test for overly eager multiple include optimization.
   Problem distilled from glibc 2.0.7's time.h, sys/time.h, timebits.h.
   Problem noted by Tom Tromey <tromey@cygnus.com>.  */
/* { dg-do compile } */

#include "cpp-mi2a.h"
#include "cpp-mi2b.h"

int main (void)
{
  return x;
}
