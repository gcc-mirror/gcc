/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
TYPE __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, int n)			\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      a[i * 2] += 10;						\
      a[i * 2 + 1] += 17;					\
    }								\
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

/* { dg-final { scan-assembler-times {\tld1rh\tz[0-9]+\.h, } 2 } } */
/* { dg-final { scan-assembler-times {\tld1rw\tz[0-9]+\.s, } 3 } } */
/* { dg-final { scan-assembler-times {\tld1rd\tz[0-9]+\.d, } 3 } } */
/* { dg-final { scan-assembler-times {\tld1rqb\tz[0-9]+\.b, } 3 } } */
/* { dg-final { scan-assembler-not {\tzip1\t} } } */
/* { dg-final { scan-assembler-not {\tzip2\t} } } */
