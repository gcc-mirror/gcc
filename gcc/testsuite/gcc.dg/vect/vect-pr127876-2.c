/* { dg-additional-options "-fno-trapping-math" } */

#include "tree-vect.h"

__attribute__((noipa))
double f (double *a, int n)
{
  double r = __builtin_nan ("");
  for (int i = 0; i < n; i++)
    if (a[i] > 100.0)
      r = -0.0;
  return r;
}

int main (void)
{
  double a[32];

  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 32; i++)
    a[i] = 1.0;
  a[7] = 200.0;

  double r = f (a, 32);
  if (__builtin_copysign (1., r) != -1.)
    __builtin_abort ();
  return 0;
}
