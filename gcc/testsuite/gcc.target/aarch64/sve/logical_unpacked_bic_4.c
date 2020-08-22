/* { dg-options "-O3 -msve-vector-bits=256" } */

#include <stdint.h>

void
f (uint16_t *restrict dst, uint8_t *restrict src1, uint8_t *restrict src2)
{
  for (int i = 0; i < 15; ++i)
    dst[i] = (uint8_t) (src1[i] & ~src2[i]);
}

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 1 } } */
