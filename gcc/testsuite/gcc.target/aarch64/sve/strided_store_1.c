/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#ifndef INDEX8
#define INDEX8 int8_t
#define INDEX16 int16_t
#define INDEX32 int32_t
#define INDEX64 int64_t
#endif

#define TEST_LOOP(DATA_TYPE, BITS)				\
  void __attribute__ ((noinline, noclone))			\
  f_##DATA_TYPE##_##BITS (DATA_TYPE *restrict dest,		\
			  DATA_TYPE *restrict src,		\
			  INDEX##BITS stride, INDEX##BITS n)	\
  {								\
    for (INDEX##BITS i = 0; i < n; ++i)				\
      dest[i * stride] = src[i] + 1;				\
  }

#define TEST_TYPE(T, DATA_TYPE)			\
  T (DATA_TYPE, 8)				\
  T (DATA_TYPE, 16)				\
  T (DATA_TYPE, 32)				\
  T (DATA_TYPE, 64)

#define TEST_ALL(T)				\
  TEST_TYPE (T, int32_t)			\
  TEST_TYPE (T, uint32_t)			\
  TEST_TYPE (T, float)				\
  TEST_TYPE (T, int64_t)			\
  TEST_TYPE (T, uint64_t)			\
  TEST_TYPE (T, double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 12 } } */
