/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_LOOP(TYPE1, TYPE2)						\
  void									\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict dst, TYPE1 *restrict src1,	\
		       TYPE2 *restrict src2, int n)			\
  {									\
    for (int i = 0; i < n; ++i)						\
      dst[i] += src1[i] + (TYPE2) (src2[i] + 1);			\
  }

#define TEST_ALL(T) \
  T (uint16_t, uint8_t) \
  T (uint32_t, uint8_t) \
  T (uint64_t, uint8_t) \
  T (uint32_t, uint16_t) \
  T (uint64_t, uint16_t) \
  T (uint64_t, uint32_t)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #1\n} 3 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tuxtw\tz[0-9]+\.d,} 1 } } */
