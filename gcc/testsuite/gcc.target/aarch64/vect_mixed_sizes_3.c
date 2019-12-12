/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+nosve"

#include <stdint.h>

void
f (int16_t *x, int16_t *y, int8_t *z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[i] += y[i];
      z[i] += z[i - 8];
    }
}

/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.8h,} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.8b,} 1 } } */
/* { dg-final { scan-assembler-not {\tadd\tv[0-9]+\.4h,} } } */
