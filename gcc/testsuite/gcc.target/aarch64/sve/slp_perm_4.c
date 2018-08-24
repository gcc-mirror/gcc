/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint8_t *restrict a, uint8_t *restrict b, uint8_t *restrict c)
{
  for (int i = 0; i < 100; ++i)
    {
      a[i * 8] = b[i * 8] + c[i * 8];
      a[i * 8 + 1] = b[i * 8] + c[i * 8 + 1];
      a[i * 8 + 2] = b[i * 8 + 2] + c[i * 8 + 2];
      a[i * 8 + 3] = b[i * 8 + 2] + c[i * 8 + 3];
      a[i * 8 + 4] = b[i * 8 + 4] + c[i * 8 + 4];
      a[i * 8 + 5] = b[i * 8 + 4] + c[i * 8 + 5];
      a[i * 8 + 6] = b[i * 8 + 6] + c[i * 8 + 6];
      a[i * 8 + 7] = b[i * 8 + 6] + c[i * 8 + 7];
    }
}

/* { dg-final { scan-assembler {\ttrn1\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} } } */
