/* { dg-do run } */
/* { dg-options "-fstrub=strict -O1" } */
/* { dg-require-effective-target strub } */

/* Check that a strub function called by another strub function does NOT defer
   the strubbing to its caller at -O1.  */

#include "strub-defer-O2.c"
