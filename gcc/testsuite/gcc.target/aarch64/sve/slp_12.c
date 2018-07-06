/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define N1 (19 * 2)

#define VEC_PERM(TYPE)						\
void __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, TYPE *restrict b)		\
{								\
  for (int i = 0; i < N1; ++i)					\
    {								\
      a[i] += 1;						\
      b[i * 4] += 2;						\
      b[i * 4 + 1] += 3;					\
      b[i * 4 + 2] += 4;					\
      b[i * 4 + 3] += 5;					\
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
  T (float)					\
  T (double)

TEST_ALL (VEC_PERM)

/* The loop should be fully-masked.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 10 } } */
/* { dg-final { scan-assembler-times {\tst1b\t} 10 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 10 } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 10 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 15 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 15 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 15 } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 15 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */
/* { dg-final { scan-assembler-not {\tstr} } } */

/* We should use WHILEs for all accesses.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 20 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 20 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 30 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 30 } } */

/* 6 for the 8-bit types and 2 for the 16-bit types.  */
/* { dg-final { scan-assembler-times {\tuqdecb\t} 8 } } */
/* 4 for the 16-bit types and 3 for the 32-bit types.  */
/* { dg-final { scan-assembler-times {\tuqdech\t} 7 } } */
/* 6 for the 32-bit types and 3 for the 64-bit types.  */
/* { dg-final { scan-assembler-times {\tuqdecw\t} 9 } } */
/* { dg-final { scan-assembler-times {\tuqdecd\t} 6 } } */
