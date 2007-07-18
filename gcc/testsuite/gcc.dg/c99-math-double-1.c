/* { dg-do run { target *-*-solaris2.1[0-9]* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  volatile double nan = NAN;
  volatile double inf = INFINITY;
  volatile double huge = HUGE_VAL;
  volatile double norm1 = __DBL_MIN__;
  volatile double norm2 = 1;
  volatile double norm3 = __DBL_MAX__;
  volatile double sub = __DBL_MIN__ / 2;
  volatile double zero = 0.0;

  C99_MATH_TESTS (nan, inf, huge, norm1, norm2, norm3, sub, zero, /*neg=*/0)
  C99_MATH_TESTS (-nan, -inf, -huge, -norm1, -norm2, -norm3, -sub, -zero, /*neg=*/1)

  return 0;
}
