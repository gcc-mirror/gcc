/* { dg-options "-O2 -ftree-vectorize -fno-tree-loop-distribute-patterns" } */

#include <stdint.h>

#define TEST_LOOP(TYPE1, TYPE2)						\
  void									\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict dst1, TYPE1 *restrict src1,	\
		       TYPE2 *restrict dst2, TYPE2 *restrict src2,	\
		       int n)						\
  {									\
    for (int i = 0; i < n; ++i)						\
      {									\
	dst1[i] += src1[i];						\
	dst2[i] = src2[i];						\
      }									\
  }

#define TEST_ALL(T) \
  T (uint16_t, uint8_t) \
  T (uint32_t, uint16_t) \
  T (uint32_t, _Float16) \
  T (uint64_t, uint32_t) \
  T (uint64_t, float)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.d,} 2 } } */

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 2 } } */
