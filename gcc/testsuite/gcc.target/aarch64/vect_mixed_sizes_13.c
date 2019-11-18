/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int8_t *x, int16_t *y, int16_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 8];
    }
}

/* { dg-final { scan-assembler-times {\txtn\tv[0-9]+\.8b, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.8h,} 1 } } */
