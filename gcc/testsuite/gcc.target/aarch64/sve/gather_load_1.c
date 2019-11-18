/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#ifndef INDEX32
#define INDEX32 int32_t
#define INDEX64 int64_t
#endif

#define TEST_LOOP(DATA_TYPE, BITS)					\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,	\
		 INDEX##BITS *indices, int n)				\
  {									\
    for (int i = 0; i < n; ++i)						\
      dest[i] += src[indices[i]];					\
  }

#define TEST_ALL(T)				\
  T (int8_t, 32)				\
  T (uint8_t, 32)				\
  T (int16_t, 32)				\
  T (uint16_t, 32)				\
  T (int32_t, 32)				\
  T (uint32_t, 32)				\
  T (float, 32)					\
  T (int64_t, 64)				\
  T (uint64_t, 64)				\
  T (double, 64)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw 1\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 3 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */
