/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

#define TEST_LOOP(DATA_TYPE, NAME, SCALE)			\
  void __attribute__ ((noinline, noclone))			\
  f_##DATA_TYPE##_##NAME (DATA_TYPE *restrict dest,		\
			  DATA_TYPE *restrict src, long n)	\
  {								\
    for (long i = 0; i < n; ++i)				\
      dest[i] += src[i * SCALE];				\
  }

#define TEST_TYPE(T, DATA_TYPE)			\
  T (DATA_TYPE, 5, 5)				\
  T (DATA_TYPE, 7, 7)				\
  T (DATA_TYPE, 11, 11)				\
  T (DATA_TYPE, 200, 200)			\
  T (DATA_TYPE, m100, -100)

#define TEST_ALL(T)				\
  TEST_TYPE (T, int32_t)			\
  TEST_TYPE (T, uint32_t)			\
  TEST_TYPE (T, float)				\
  TEST_TYPE (T, int64_t)			\
  TEST_TYPE (T, uint64_t)			\
  TEST_TYPE (T, double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, uxtw\]\n} 12 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+.d\]\n} 15 } } */
