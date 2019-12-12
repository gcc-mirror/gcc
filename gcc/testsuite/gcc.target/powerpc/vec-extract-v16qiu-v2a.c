/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE unsigned char
/* ELEMENTS is number of elements in a vector of TYPE.  */
#define ELEMENTS 16
#define INITIAL \
  {  3, 2, 3, 4, 5, 6, 7, 8, 240, 241, 242, 243, 244, 245, 246, 247 }

#define DO_TRACE
#define DISABLE_INLINE_OF_GET_AUTO_N

#include "vec-extract-v16qiu-v2.h"
