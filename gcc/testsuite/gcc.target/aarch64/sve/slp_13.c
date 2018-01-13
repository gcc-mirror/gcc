/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
TYPE __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, int n)			\
{								\
  TYPE res = 0;							\
  for (int i = 0; i < n; ++i)					\
    {								\
      res += a[i * 2] * 3;					\
      res += a[i * 2 + 1] * 5;					\
    }								\
  return res;							\
}

#define TEST_ALL(T)				\
  T (int8_t)					\
  T (uint8_t)					\
  T (int16_t)					\
  T (uint16_t)					\
  T (int32_t)					\
  T (uint32_t)					\
  T (int64_t)					\
  T (uint64_t)

TEST_ALL (VEC_PERM)

/* ??? We don't treat the int8_t and int16_t loops as reductions.  */
/* ??? We don't treat the uint loops as SLP.  */
/* The loop should be fully-masked.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 1 } } */
/* { dg-final { scan-assembler-not {\tldr} { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 4 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 4 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 4 } } */

/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.b\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.h\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-not {\tuqdec} } } */
