/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#define TEST_LOOP(DATA_TYPE)						\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict *src,	\
		 int n)							\
  {									\
    for (int i = 0; i < n; ++i)						\
      dest[i] += *src[i];						\
  }

#define TEST_ALL(T)				\
  T (int8_t)					\
  T (uint8_t)					\
  T (int16_t)					\
  T (uint16_t)					\
  T (int32_t)					\
  T (uint32_t)					\
  T (int64_t)					\
  T (uint64_t)					\
  T (double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+.d\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+.d\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+.d\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+.d\]\n} 3 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */
