/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#ifndef INDEX32
#define INDEX32 int32_t
#define INDEX64 int64_t
#endif

/* Invoked 18 times for each data size.  */
#define TEST_LOOP(DATA_TYPE, BITS)					\
  void __attribute__ ((noinline, noclone))				\
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,	\
		 INDEX##BITS *indices, int n)				\
  {									\
    for (int i = 9; i < n; ++i)						\
      *(DATA_TYPE *) ((char *) dest + indices[i]) = src[i] + 1;		\
  }

#define TEST_ALL(T)				\
  T (int32_t, 32)				\
  T (uint32_t, 32)				\
  T (float, 32)					\
  T (int64_t, 64)				\
  T (uint64_t, 64)				\
  T (double, 64)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, sxtw\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d\]\n} 3 } } */
