/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=512" } */

#include <stdint.h>

void
f (uint64_t *dst, uint32_t *restrict src1, uint16_t *restrict src2,
   uint8_t *restrict src3)
{
  for (int i = 0; i < 7; ++i)
    dst[i] += (uint32_t) (src1[i] + (uint16_t) (src2[i]
						+ (uint8_t) (src3[i] + 1)));
}

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxtw\tz[0-9]+\.d,} 1 } } */
