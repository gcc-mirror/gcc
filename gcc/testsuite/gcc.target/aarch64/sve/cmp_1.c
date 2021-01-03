/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_PAIR(TYPE1, TYPE2)				\
  void							\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict x,		\
		       TYPE2 *restrict g, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      if (g[i] < 4)					\
	x[i] += 1;					\
  }

#define TEST_SINGLE(TYPE)			\
  TEST_PAIR (TYPE, int8_t)			\
  TEST_PAIR (TYPE, uint8_t)			\
  TEST_PAIR (TYPE, int16_t)			\
  TEST_PAIR (TYPE, uint16_t)			\
  TEST_PAIR (TYPE, int32_t)			\
  TEST_PAIR (TYPE, uint32_t)			\
  TEST_PAIR (TYPE, int64_t)			\
  TEST_PAIR (TYPE, uint64_t)

TEST_SINGLE (int8_t)
TEST_SINGLE (uint8_t)
TEST_SINGLE (int16_t)
TEST_SINGLE (uint16_t)
TEST_SINGLE (int32_t)
TEST_SINGLE (uint32_t)
TEST_SINGLE (int64_t)
TEST_SINGLE (uint64_t)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.b,} 8 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h,} 8 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s,} 8 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 8 } } */

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h,} 16 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s,} 8 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 8 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s,} 24 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 8 } } */

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 32 } } */

/* { dg-final { scan-assembler-times {\tcmpl[et]\tp[0-9]+\.b,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[so]\tp[0-9]+\.b,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[et]\tp[0-9]+\.h,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[so]\tp[0-9]+\.h,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[et]\tp[0-9]+\.s,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[so]\tp[0-9]+\.s,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[et]\tp[0-9]+\.d,} 8 } } */
/* { dg-final { scan-assembler-times {\tcmpl[so]\tp[0-9]+\.d,} 8 } } */

/* { dg-final { scan-assembler-not {\tpunpk} } } */
