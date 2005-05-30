/* { dg-do run { target *-*-solaris2.1[0-9]* } } */
/* { dg-options "-std=c99 -O" } */

#include <math.h>
#include "c99-math.h"

int main(void)
{
  long double nan = NAN;
  long double inf = INFINITY;
  long double huge = HUGE_VALL;
  long double norm = __LDBL_MIN__;
  long double zero = 0.0l;

  C99_MATH_TESTS (nan, inf, huge, norm, zero)

  return 0;
}
