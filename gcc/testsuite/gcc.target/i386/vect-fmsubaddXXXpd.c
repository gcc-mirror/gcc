/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O3 -mfma -save-temps" } */

#include "fma-check.h"

void __attribute__((noipa))
check_fmsubadd (double * __restrict a, double *b, double *c, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[2*i + 0] = b[2*i + 0] * c[2*i + 0] + a[2*i + 0];
      a[2*i + 1] = b[2*i + 1] * c[2*i + 1] - a[2*i + 1];
    }
}

static void
fma_test (void)
{
  double a[4], b[4], c[4];
  for (int i = 0; i < 4; ++i)
    {
      a[i] = i;
      b[i] = 3*i;
      c[i] = 7*i;
    }
  check_fmsubadd (a, b, c, 2);
  const double d[4] = { 0., 20., 86., 186. };
  for (int i = 0; i < 4; ++i)
    if (a[i] != d[i])
      __builtin_abort ();
}

/* { dg-final { scan-assembler "fmsubadd...pd" } } */
