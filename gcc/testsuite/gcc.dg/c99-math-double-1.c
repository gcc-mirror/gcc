/* { dg-do run { target *-*-solaris2.1[0-9]* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  double nan = NAN;
  double inf = INFINITY;
  double huge = HUGE_VAL;
  double norm = __DBL_MIN__;
  double zero = 0.0;

  C99_MATH_TESTS (nan, inf, huge, norm, zero)

  return 0;
}
