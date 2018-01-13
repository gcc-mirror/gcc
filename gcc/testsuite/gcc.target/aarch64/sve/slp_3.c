/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
TYPE __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, int n)			\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      a[i * 4] += 41;						\
      a[i * 4 + 1] += 25;					\
      a[i * 4 + 2] += 31;					\
      a[i * 4 + 3] += 62;					\
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

/* 1 for each 8-bit type.  */
/* { dg-final { scan-assembler-times {\tld1rw\tz[0-9]+\.s, } 2 } } */
/* 1 for each 16-bit type and 4 for double.  */
/* { dg-final { scan-assembler-times {\tld1rd\tz[0-9]+\.d, } 7 } } */
/* 1 for each 32-bit type.  */
/* { dg-final { scan-assembler-times {\tld1rqb\tz[0-9]+\.b, } 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #41\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #25\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #31\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #62\n} 2 } } */
/* The 64-bit types need:

      ZIP1 ZIP1 (2 ZIP2s optimized away)
      ZIP1 ZIP2.  */
/* { dg-final { scan-assembler-times {\tzip1\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 9 } } */
/* { dg-final { scan-assembler-times {\tzip2\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
