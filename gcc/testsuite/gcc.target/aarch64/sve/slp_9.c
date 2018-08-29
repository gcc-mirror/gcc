/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define VEC_PERM(TYPE1, TYPE2)					\
void __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE1##_##TYPE2 (TYPE1 *restrict a,			\
			   TYPE2 *restrict b, int n)		\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      a[i * 2] += 1;						\
      a[i * 2 + 1] += 2;					\
      b[i * 2] += 3;						\
      b[i * 2 + 1] += 4;					\
    }								\
}

#define TEST_ALL(T)				\
  T (int8_t, uint16_t)				\
  T (uint8_t, int16_t)				\
  T (int16_t, uint32_t)				\
  T (uint16_t, int32_t)				\
  T (int32_t, double)				\
  T (uint32_t, int64_t)				\
  T (float, uint64_t)

TEST_ALL (VEC_PERM)

/* The loop should be fully-masked.  The load XFAILs for fixed-length
   SVE account for extra loads from the constant pool.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 { xfail { aarch64_sve && { ! vect_variable_length } } } } }*/
/* { dg-final { scan-assembler-times {\tst1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 6 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 6 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 7 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 7 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 6 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 6 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */
/* { dg-final { scan-assembler-not {\tstr} } } */

/* We should use WHILEs for the accesses to "a" and unpacks for the accesses
   to "b".  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 6 } } */
/* { dg-final { scan-assembler-not {\twhilelo\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-times {\tpunpklo\tp[0-7]\.h} 7 } } */
/* { dg-final { scan-assembler-times {\tpunpkhi\tp[0-7]\.h} 7 } } */

/* { dg-final { scan-assembler-not {\tuqdec} } } */
