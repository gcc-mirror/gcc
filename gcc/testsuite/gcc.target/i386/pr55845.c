/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -ffast-math -fschedule-insns -mavx -mvzeroupper" } */

#include "avx-check.h"

#define N 100

double
__attribute__((noinline))
foo (int size, double *y, double *x)
{
  double sum = 0.0;
  int i;

  for (i = 0; i < size; i++)
    sum += y[i] * x[i];

  return sum;
}

static void
__attribute__ ((noinline))
avx_test ()
{
  double x[N], y[N];
  double s;
  int i;

  for (i = 0; i < N; i++)
    {
      x[i] = i;
      y[i] = i;
    }

  s = foo (N, y, x);

  if (s != 328350.0)
    abort ();
}
