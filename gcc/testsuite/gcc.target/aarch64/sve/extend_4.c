/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=512" } */

#include <stdint.h>

void
f (int64_t *dst, int32_t *restrict src1, int16_t *restrict src2,
   int8_t *restrict src3)
{
  for (int i = 0; i < 7; ++i)
    dst[i] += (int32_t) (src1[i] + (int16_t) (src2[i]
					      + (int8_t) (src3[i] + 1)));
}

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsxtb\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tsxth\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tsxtw\tz[0-9]+\.d,} 1 } } */
