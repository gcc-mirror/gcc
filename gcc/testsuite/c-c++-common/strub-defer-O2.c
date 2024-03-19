/* { dg-do run } */
/* { dg-options "-fstrub=strict -O2" } */
/* { dg-require-effective-target strub } */

/* Check that a strub function called by another strub function does NOT defer
   the strubbing to its caller at -O2.  */

#define EXPECT_DEFERRAL !
#include "strub-defer-O3.c"
