/* { dg-do run { target { *-*-linux* *-*-gnu* } } } */
/* { dg-options "-O0 -lm" } */
/* { dg-require-effective-target issignaling } */

#define _GNU_SOURCE
#include <math.h>

int main (void)
{
  float sNaN = __builtin_nansf ("");
  double x = (double) sNaN;
  if (issignaling(x))
  {
    __builtin_abort();
  }

  return 0;
}
