/* { dg-do run { target *-*-solaris2.1[0-9]* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  volatile float nan = NAN;
  volatile float inf = INFINITY;
  volatile float huge = HUGE_VALF;
  volatile float norm1 = __FLT_MIN__;
  volatile float norm2 = 1;
  volatile float norm3 = __FLT_MAX__;
  volatile float sub = __FLT_MIN__ / 2;
  volatile float zero = 0.0f;

  C99_MATH_TESTS (nan, inf, huge, norm1, norm2, norm3, sub, zero, /*neg=*/0)
  C99_MATH_TESTS (-nan, -inf, -huge, -norm1, -norm2, -norm3, -sub, -zero, /*neg=*/1)

  return 0;
}
