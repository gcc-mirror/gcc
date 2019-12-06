/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int16_t *x, int32_t *y, int32_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 4];
    }
}

/* { dg-final { scan-assembler-times {\txtn\tv[0-9]+\.4h, v[0-9]+\.4s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.4s,} 1 } } */
