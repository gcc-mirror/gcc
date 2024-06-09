/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --save-temps --param aarch64-vect-compare-costs=0" } */

#include <stdint.h>

#define TEST_LOOP(DATA_TYPE, CMP_TYPE, INDEX_TYPE)			\
  void									\
  f_##DATA_TYPE##_##CMP_TYPE##_##INDEX_TYPE				\
    (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,			\
     CMP_TYPE *cmp1, CMP_TYPE *cmp2, INDEX_TYPE *indices, int n)	\
  {									\
    for (int i = 0; i < n; ++i)						\
      if (cmp1[i] == cmp2[i])						\
	dest[i] += src[indices[i]];					\
  }

#define TEST32(T, DATA_TYPE)			\
  T (DATA_TYPE, int64_t, int32_t)		\
  T (DATA_TYPE, uint64_t, int32_t)		\
  T (DATA_TYPE, double, int32_t)		\
  T (DATA_TYPE, int64_t, uint32_t)		\
  T (DATA_TYPE, uint64_t, uint32_t)		\
  T (DATA_TYPE, double, uint32_t)

#define TEST_ALL(T)			\
  TEST32 (T, int32_t)			\
  TEST32 (T, uint32_t)			\
  TEST32 (T, float)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 3\]\n} 72 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 24 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 12 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 36 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, sxtw 2\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, uxtw 2\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, x[0-9]+, lsl 2\]\n} 18 } } */
