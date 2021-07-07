/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O3 -mfma -save-temps -mavx512f -mprefer-vector-width=512" } */

#include "fma-check.h"
void __attribute__((noipa))
check_fmaddsub (double * __restrict a, double *b, double *c, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[8*i + 0] = b[8*i + 0] * c[8*i + 0] - a[8*i + 0];
      a[8*i + 1] = b[8*i + 1] * c[8*i + 1] + a[8*i + 1];
      a[8*i + 2] = b[8*i + 2] * c[8*i + 2] - a[8*i + 2];
      a[8*i + 3] = b[8*i + 3] * c[8*i + 3] + a[8*i + 3];
      a[8*i + 4] = b[8*i + 4] * c[8*i + 4] - a[8*i + 4];
      a[8*i + 5] = b[8*i + 5] * c[8*i + 5] + a[8*i + 5];
      a[8*i + 6] = b[8*i + 6] * c[8*i + 6] - a[8*i + 6];
      a[8*i + 7] = b[8*i + 7] * c[8*i + 7] + a[8*i + 7];
    }
}

static void
fma_test (void)
{
  if (!__builtin_cpu_supports ("avx512f"))
    return;
  double a[8], b[8], c[8];
  for (int i = 0; i < 8; ++i)
    {
      a[i] = i;
      b[i] = 3*i;
      c[i] = 7*i;
    }
  check_fmaddsub (a, b, c, 1);
  const double d[8] = { 0., 22., 82., 192., 332., 530., 750., 1036.};
  for (int i = 0; i < 8; ++i)
    if (a[i] != d[i])
      __builtin_abort ();
}

/* { dg-final { scan-assembler {(?n)fmaddsub...pd[ \t].*%zmm[0-9]} } } */
