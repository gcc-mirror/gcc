/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fwrapv --save-temps --param aarch64-sve-compare-costs=0" } */

#include <stdint.h>

#ifndef INDEX32
#define INDEX16 int16_t
#define INDEX32 int32_t
#endif

/* Invoked 18 times for each data size.  */
#define TEST_LOOP(DATA_TYPE, BITS)					\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,	\
		 INDEX##BITS *indices, INDEX##BITS mask, int n)		\
  {									\
    for (int i = 9; i < n; ++i)						\
      dest[(INDEX##BITS) (indices[i] | mask)] = src[i] + 1;		\
  }

#define TEST_ALL(T)				\
  T (int32_t, 16)				\
  T (uint32_t, 16)				\
  T (float, 16)					\
  T (int64_t, 32)				\
  T (uint64_t, 32)				\
  T (double, 32)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tsunpkhi\tz[0-9]+\.s, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsunpklo\tz[0-9]+\.s, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsunpkhi\tz[0-9]+\.d, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsunpklo\tz[0-9]+\.d, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 6 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 6 } } */
