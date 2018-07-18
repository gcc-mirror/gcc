/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE signed char
#define RTYPE double
#define FAIL_FORMAT "%g"
#define FAIL_CAST(X) ((double)(X))
#define ELEMENTS 16
#define INITIAL \
  {  10,  -20,  30,  -40, 50, -60, 70, -80, \
     90, -100, 110, -120, 30, -40, 50, -60 }

#include "vec-extract.h"
