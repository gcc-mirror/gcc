/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE unsigned short
#define RTYPE double
#define FAIL_FORMAT "%g"
#define FAIL_CAST(X) ((double)(X))
#define ELEMENTS 8
#define INITIAL { 1, 2, 3, 4, 0xf1, 0xf2, 0xf3, 0xf4 }

#include "vec-extract.h"
