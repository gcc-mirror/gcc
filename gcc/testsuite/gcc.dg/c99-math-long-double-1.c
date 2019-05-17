/* { dg-do run { target *-*-solaris2* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  volatile long double nan = NAN;
  volatile long double inf = INFINITY;
  volatile long double huge = HUGE_VALL;
  volatile long double norm1 = __LDBL_MIN__;
  volatile long double norm2 = 1;
  volatile long double norm3 = __LDBL_MAX__;
  volatile long double sub = __LDBL_MIN__ / 2;
  volatile long double zero = 0.0l;

  C99_MATH_TESTS (nan, inf, huge, norm1, norm2, norm3, sub, zero, /*neg=*/0)
  C99_MATH_TESTS (-nan, -inf, -huge, -norm1, -norm2, -norm3, -sub, -zero, /*neg=*/1)

  return 0;
}
