/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#define TEST_LOOP(DATA_TYPE, OTHER_TYPE)				\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE##_##BITS (DATA_TYPE *restrict dest,			\
			  DATA_TYPE *restrict src,			\
			  OTHER_TYPE *restrict other,			\
			  OTHER_TYPE mask,				\
			  int stride, int n)				\
  {									\
    for (int i = 0; i < n; ++i)						\
      dest[i] = src[i * stride] + (OTHER_TYPE) (other[i] | mask);	\
  }

#define TEST_ALL(T)				\
  T (int32_t, int16_t)				\
  T (uint32_t, int16_t)				\
  T (float, int16_t)				\
  T (int64_t, int32_t)				\
  T (uint64_t, int32_t)				\
  T (double, int32_t)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 1\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 6 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 6 } } */
