/* { dg-do run { target *-*-solaris2.1[0-9]* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  float nan = NAN;
  float inf = INFINITY;
  float huge = HUGE_VALF;
  float norm = __FLT_MIN__;
  float zero = 0.0f;

  C99_MATH_TESTS (nan, inf, huge, norm, zero)

  return 0;
}
