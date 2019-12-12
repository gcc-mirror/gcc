/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint8_t *restrict a, uint8_t *restrict b)
{
  for (int i = 0; i < 100; ++i)
    {
      a[i * 8] = b[i * 8 + 3] + 1;
      a[i * 8 + 1] = b[i * 8 + 2] + 2;
      a[i * 8 + 2] = b[i * 8 + 1] + 3;
      a[i * 8 + 3] = b[i * 8 + 0] + 4;
      a[i * 8 + 4] = b[i * 8 + 7] + 5;
      a[i * 8 + 5] = b[i * 8 + 6] + 6;
      a[i * 8 + 6] = b[i * 8 + 5] + 7;
      a[i * 8 + 7] = b[i * 8 + 4] + 8;
    }
}

/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
