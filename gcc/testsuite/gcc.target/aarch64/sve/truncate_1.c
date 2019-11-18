/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_LOOP(TYPE1, TYPE2, SHIFT)					\
  void									\
  f_##TYPE1##_##TYPE2 (TYPE2 *restrict dst, TYPE1 *restrict src1,	\
		       TYPE1 *restrict src2, int n)			\
  {									\
    for (int i = 0; i < n; ++i)						\
      dst[i] = (TYPE1) (src1[i] + src2[i]) >> SHIFT;			\
  }

#define TEST_ALL(T) \
  T (uint16_t, uint8_t, 2) \
  T (uint32_t, uint8_t, 18) \
  T (uint64_t, uint8_t, 34) \
  T (uint32_t, uint16_t, 3) \
  T (uint64_t, uint16_t, 19) \
  T (uint64_t, uint32_t, 4)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s,} 4 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 6 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.h, z[0-9]+\.h, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.s, z[0-9]+\.s, #18\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, z[0-9]+\.d, #34\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.s, z[0-9]+\.s, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, z[0-9]+\.d, #19\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, z[0-9]+\.d, #4\n} 1 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.d,} 1 } } */
