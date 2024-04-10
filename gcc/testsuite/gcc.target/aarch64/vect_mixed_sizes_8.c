/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int64_t *x, int64_t *y, uint32_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 2];
    }
}

/* { dg-final { scan-assembler-times {\tzip1\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.2d,} 1 } } */
