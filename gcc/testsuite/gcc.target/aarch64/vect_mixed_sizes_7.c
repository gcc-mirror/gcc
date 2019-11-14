/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int16_t *x, int16_t *y, int8_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 8];
    }
}

/* { dg-final { scan-assembler-times {\tsxtl\tv[0-9]+\.8h, v[0-9]+\.8b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.8h,} 1 } } */
