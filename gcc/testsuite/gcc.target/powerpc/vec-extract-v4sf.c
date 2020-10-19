/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE float
#define FAIL_FORMAT "%g"
#define FAIL_CAST(X) ((double)(X))
#define ELEMENTS 4
#define INITIAL { 10.0f, -20.0f, 30.0f, -40.0f }

#include "vec-extract.h"
