/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
void __attribute__ ((weak))					\
vec_slp_##TYPE (TYPE *restrict a, TYPE *restrict b, int n)	\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      TYPE a1 = a[i * 2];					\
      TYPE a2 = a[i * 2 + 1];					\
      TYPE b1 = b[i * 2];					\
      TYPE b2 = b[i * 2 + 1];					\
      a[i * 2] = b1 > 1 ? a1 / b1 : a1;				\
      a[i * 2 + 1] = b2 > 2 ? a2 / b2 : a2;			\
    }								\
}

#define TEST_ALL(T)				\
  T (int32_t)					\
  T (uint32_t)					\
  T (int64_t)					\
  T (uint64_t)					\
  T (float)					\
  T (double)

TEST_ALL (VEC_PERM)

/* The loop should be fully-masked.  The load XFAILs for fixed-length
   SVE account for extra loads from the constant pool.  */
/* { dg-final { scan-assembler-times {\tld1w\t} 6 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 6 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 3 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */
/* { dg-final { scan-assembler-not {\tstr} } } */

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 6 } } */

/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.d} 1 } } */
