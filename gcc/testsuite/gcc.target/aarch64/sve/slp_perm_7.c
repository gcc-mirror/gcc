/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint8_t *restrict a, uint8_t *restrict b)
{
  for (int i = 0; i < 100; ++i)
    {
      a[i * 8] = b[i * 8 + 1] + 1;
      a[i * 8 + 1] = b[i * 8 + 7] + 2;
      a[i * 8 + 2] = b[i * 8 + 1] + 3;
      a[i * 8 + 3] = b[i * 8 + 7] + 4;
      a[i * 8 + 4] = b[i * 8 + 1] + 5;
      a[i * 8 + 5] = b[i * 8 + 7] + 6;
      a[i * 8 + 6] = b[i * 8 + 1] + 7;
      a[i * 8 + 7] = b[i * 8 + 7] + 8;
    }
}

/* { dg-final { scan-assembler {\ttbl\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} } } */
