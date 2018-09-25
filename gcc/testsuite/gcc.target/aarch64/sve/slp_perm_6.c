/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint8_t *restrict a, uint8_t *restrict b)
{
  for (int i = 0; i < 100; ++i)
    {
      a[i * 8] = b[i * 8 + 3] + 1;
      a[i * 8 + 1] = b[i * 8 + 6] + 1;
      a[i * 8 + 2] = b[i * 8 + 0] + 1;
      a[i * 8 + 3] = b[i * 8 + 2] + 1;
      a[i * 8 + 4] = b[i * 8 + 1] + 1;
      a[i * 8 + 5] = b[i * 8 + 7] + 1;
      a[i * 8 + 6] = b[i * 8 + 5] + 1;
      a[i * 8 + 7] = b[i * 8 + 4] + 1;
    }
}

/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
