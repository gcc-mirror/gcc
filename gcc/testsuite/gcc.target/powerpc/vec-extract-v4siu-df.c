/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE unsigned int
#define RTYPE double
#define FAIL_FORMAT "%g"
#define FAIL_CAST(X) ((double)(X))
#define ELEMENTS 4
#define INITIAL { 1, 2, 0xff03, 0xff04 }

#include "vec-extract.h"
