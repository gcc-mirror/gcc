/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_LOOP(TYPE1, TYPE2)						\
  void									\
  f_##TYPE1##_##TYPE2 (TYPE1 *restrict dst, TYPE1 *restrict src1,	\
		       TYPE2 *restrict src2, int32_t *restrict index,	\
		       int n)						\
  {									\
    for (int i = 0; i < n; ++i)						\
      dst[i] += src1[i] + src2[index[i]];				\
  }

#define TEST_ALL(T) \
  T (uint16_t, uint8_t) \
  T (uint32_t, uint8_t) \
  T (uint64_t, uint8_t) \
  T (uint32_t, uint16_t) \
  T (uint64_t, uint16_t) \
  T (uint64_t, uint32_t)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, sxtw\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, sxtw 1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d, lsl 1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d, lsl 2\]\n} 1 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 7 } } */
/* { dg-final { scan-assembler-times {\tld1sw\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 3 } } */

/* { dg-final { scan-assembler-not {\tuxt.\t} } } */
