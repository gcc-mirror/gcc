/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fwrapv --save-temps" } */

#include <stdint.h>

#ifndef INDEX32
#define INDEX16 int16_t
#define INDEX32 int32_t
#endif

#define TEST_LOOP(DATA_TYPE, BITS)					\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,	\
		 INDEX##BITS *indices, INDEX##BITS mask, int n)		\
  {									\
    for (int i = 0; i < n; ++i)						\
      dest[i] = src[(INDEX##BITS) (indices[i] + mask)];			\
  }

#define TEST_ALL(T)				\
  T (int8_t, 16)				\
  T (uint8_t, 16)				\
  T (int16_t, 16)				\
  T (uint16_t, 16)				\
  T (_Float16, 16)				\
  T (int32_t, 16)				\
  T (uint32_t, 16)				\
  T (float, 16)					\
  T (int64_t, 32)				\
  T (uint64_t, 32)				\
  T (double, 32)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw 1\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+.d, sxtw 3\]\n} 3 } } */

/* { dg-final { scan-assembler-times {\tsxt.\tz} 8 } } */
/* { dg-final { scan-assembler-times {\tsxth\tz[0-9]+\.s,} 8 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */
