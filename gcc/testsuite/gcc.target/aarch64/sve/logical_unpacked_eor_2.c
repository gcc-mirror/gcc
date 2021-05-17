/* { dg-options "-O3 -msve-vector-bits=256" } */

#include <stdint.h>

void
f (uint64_t *restrict dst, uint16_t *restrict src1, uint8_t *restrict src2)
{
  for (int i = 0; i < 7; ++i)
    dst[i] = (uint16_t) (src1[i] ^ src2[i]);
}

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-not {\tuxtb\tz[0-9]+\.h,} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 2 } } */
