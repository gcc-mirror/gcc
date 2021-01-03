/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_PAIR(TYPE1, TYPE2)					\
  void								\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict x, TYPE1 y, TYPE1 z,	\
		       TYPE2 *restrict g, TYPE2 h, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      x[i] = g[i] < h ? y : z;					\
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
TEST_SINGLE (float)
TEST_SINGLE (int64_t)
TEST_SINGLE (uint64_t)
TEST_SINGLE (double)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.b,} 4 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h,} 4 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s,} 6 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d,} 6 } } */

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h,} 8 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s,} 6 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d,} 6 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s,} 14 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d,} 6 } } */

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 20 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.h,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.d,} 4 } } */

/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 8 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 4 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.d,} 4 } } */

/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 18 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.d,} 6 } } */

/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 24 } } */

/* { dg-final { scan-assembler-times {\tcmp(?:h[is]|l[os])\tp[0-9]+\.b,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp[lg][et]\tp[0-9]+\.b,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp(?:h[is]|l[os])\tp[0-9]+\.h,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp[lg][et]\tp[0-9]+\.h,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp(?:h[is]|l[os])\tp[0-9]+\.s,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp[lg][et]\tp[0-9]+\.s,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp(?:h[is]|l[os])\tp[0-9]+\.d,} 10 } } */
/* { dg-final { scan-assembler-times {\tcmp[lg][et]\tp[0-9]+\.d,} 10 } } */

/* { dg-final { scan-assembler-not {\tpunpk} } } */
