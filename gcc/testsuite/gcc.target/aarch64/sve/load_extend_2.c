/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_LOOP(TYPE1, TYPE2)						\
  void									\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict dst, TYPE1 *restrict src1,	\
		       TYPE2 *restrict src2, int n)			\
  {									\
    for (int i = 0; i < n; ++i)						\
      dst[i] += src1[i] + src2[i];					\
  }

#define TEST_ALL(T) \
  T (int16_t, int8_t) \
  T (int32_t, int8_t) \
  T (int64_t, int8_t) \
  T (int32_t, int16_t) \
  T (int64_t, int16_t) \
  T (int64_t, int32_t)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1sb\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1sb\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1sb\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1sh\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1sh\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1sw\tz[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-not {\tsxt.\t} } } */
