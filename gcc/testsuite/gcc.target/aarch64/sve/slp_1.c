/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
TYPE __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, TYPE b, TYPE c, int n)	\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      a[i * 2] += b;						\
      a[i * 2 + 1] += c;					\
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

/* We should use one DUP for each of the 8-, 16- and 32-bit types,
   although we currently use LD1RW for _Float16.  We should use two
   DUPs for each of the three 64-bit types.  */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, [hw]} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, [sw]} 2 } } */
/* { dg-final { scan-assembler-times {\tld1rw\tz[0-9]+\.s, } 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, [dx]} 9 } } */
/* { dg-final { scan-assembler-times {\tzip1\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-not {\tzip2\t} } } */

/* The loop should be fully-masked.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tst1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 3 } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 3 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 6 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */
/* { dg-final { scan-assembler-times {\tstr} 2 } } */
/* { dg-final { scan-assembler-times {\tstr\th[0-9]+} 2 } } */

/* { dg-final { scan-assembler-not {\tuqdec} } } */
