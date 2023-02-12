/* { dg-do compile } */
/* The cost model thinks that the double loop isn't a win for SVE-128.  */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable -fno-vect-cost-model" } */

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
  T (uint64_t)					\
  T (_Float16)					\
  T (float)					\
  T (double)

TEST_ALL (VEC_PERM)

/* The loop should be fully-masked.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 3 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 6 } } */

/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadda\th[0-9]+, p[0-7], h[0-9]+, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-not {\tfadd\n} } } */

/* { dg-final { scan-assembler-not {\tuqdec} } } */
