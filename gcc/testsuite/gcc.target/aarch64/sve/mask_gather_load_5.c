/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --save-temps" } */

#include <stdint.h>

#ifndef INDEX32
#define INDEX32 int32_t
#define INDEX64 int64_t
#endif

#define TEST_LOOP(DATA_TYPE, CMP_TYPE)					\
  void									\
  f_##DATA_TYPE##_##CMP_TYPE						\
    (DATA_TYPE *restrict dest, DATA_TYPE *restrict *restrict src,	\
     CMP_TYPE *cmp1, CMP_TYPE *cmp2, int n)				\
  {									\
    for (int i = 0; i < n; ++i)						\
      if (cmp1[i] == cmp2[i])						\
	dest[i] += *src[i];						\
  }

#define TEST_TYPE(T, DATA_TYPE)		\
  T (DATA_TYPE, int64_t)		\
  T (DATA_TYPE, uint64_t)		\
  T (DATA_TYPE, double)

#define TEST_ALL(T)			\
  TEST_TYPE (T, int64_t)		\
  TEST_TYPE (T, uint64_t)		\
  TEST_TYPE (T, double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 3\]\n} 36 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+\.d\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, x[0-9]+, lsl 3\]\n} 9 } } */
