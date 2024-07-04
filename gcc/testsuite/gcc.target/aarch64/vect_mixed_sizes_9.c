/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int32_t *x, int32_t *y, uint16_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] = z[i];
      y[i] += y[i - 4];
    }
}

/* { dg-final { scan-assembler-times {\tzip1\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.4s,} 1 } } */
