/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512 -save-temps" } */

#include "fma-check.h"
void __attribute__((noipa))
check_fmaddsub (float * __restrict a, float *b, float *c, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[16*i + 0] = b[16*i + 0] * c[16*i + 0] + a[16*i + 0];
      a[16*i + 1] = b[16*i + 1] * c[16*i + 1] - a[16*i + 1];
      a[16*i + 2] = b[16*i + 2] * c[16*i + 2] + a[16*i + 2];
      a[16*i + 3] = b[16*i + 3] * c[16*i + 3] - a[16*i + 3];
      a[16*i + 4] = b[16*i + 4] * c[16*i + 4] + a[16*i + 4];
      a[16*i + 5] = b[16*i + 5] * c[16*i + 5] - a[16*i + 5];
      a[16*i + 6] = b[16*i + 6] * c[16*i + 6] + a[16*i + 6];
      a[16*i + 7] = b[16*i + 7] * c[16*i + 7] - a[16*i + 7];
      a[16*i + 8] = b[16*i + 8] * c[16*i + 8] + a[16*i + 8];
      a[16*i + 9] = b[16*i + 9] * c[16*i + 9] - a[16*i + 9];
      a[16*i + 10] = b[16*i + 10] * c[16*i + 10] + a[16*i + 10];
      a[16*i + 11] = b[16*i + 11] * c[16*i + 11] - a[16*i + 11];
      a[16*i + 12] = b[16*i + 12] * c[16*i + 12] + a[16*i + 12];
      a[16*i + 13] = b[16*i + 13] * c[16*i + 13] - a[16*i + 13];
      a[16*i + 14] = b[16*i + 14] * c[16*i + 14] + a[16*i + 14];
      a[16*i + 15] = b[16*i + 15] * c[16*i + 15] - a[16*i + 15];
    }
}

static void
fma_test (void)
{
  if (!__builtin_cpu_supports ("avx512f"))
    return;
  float a[16], b[16], c[16];
  for (int i = 0; i < 16; ++i)
    {
      a[i] = i;
      b[i] = 3*i;
      c[i] = 7*i;
    }
  check_fmaddsub (a, b, c, 1);
  const float d[16] = { 0., 20., 86., 186., 340., 520., 762., 1022.,
			1352, 1692., 2110., 2530., 3036., 3536., 4130., 4710.};
  for (int i = 0; i < 16; ++i)
    if (a[i] != d[i])
      __builtin_abort ();
}

/* { dg-final { scan-assembler {(?n)fmsubadd...ps[ \t].*%zmm[0-9]} } } */
