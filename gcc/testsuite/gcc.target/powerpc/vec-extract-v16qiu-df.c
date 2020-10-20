/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE unsigned char
#define RTYPE double
#define FAIL_FORMAT "%g"
#define FAIL_CAST(X) ((double)(X))
#define ELEMENTS 16
#define INITIAL \
  {  1, 2, 3, 4, 5, 6, 7, 8, 240, 241, 242, 243, 244, 245, 246, 247 }

#include "vec-extract.h"
