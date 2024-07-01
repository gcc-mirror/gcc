/* { dg-additional-options "-ffast-math -fno-unsafe-math-optimizations" } */

#include "tree-vect.h"

double __attribute__((noipa))
foo (double *x, double *y, int n)
{
  double res = 0.;
  for (int i = 0; i < n; ++i)
    if (y[i] > 0.)
      res += x[i];
    else
      res = 64.;
  return res;
}

double y[16] = { 1., 1., 1., 1., 0., 1., 1., 1.,
                 1., 1., 1., 1., 1., 1., 1., 1. };
int main ()
{
  check_vect ();
  if (foo (y, y, 16) != 64. + 11.)
    abort ();
  return 0;
}
