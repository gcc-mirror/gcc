/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int32_t *x, int64_t *y, int64_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 2];
    }
}

/* { dg-final { scan-assembler-times {\txtn\tv[0-9]+\.2s, v[0-9]+\.2d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.2d,} 1 } } */
