/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
TYPE __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, int n)			\
{								\
  for (int i = 0; i < n; ++i)					\
    {								\
      a[i * 8] += 99;						\
      a[i * 8 + 1] += 11;					\
      a[i * 8 + 2] += 17;					\
      a[i * 8 + 3] += 80;					\
      a[i * 8 + 4] += 63;					\
      a[i * 8 + 5] += 37;					\
      a[i * 8 + 6] += 24;					\
      a[i * 8 + 7] += 81;					\
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

/* 1 for each 8-bit type, 4 for each 32-bit type and 8 for double.  */
/* { dg-final { scan-assembler-times {\tld1rd\tz[0-9]+\.d, } 22 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tld1rqb\tz[0-9]+\.b, } 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tld1rd\tz[0-9]+\.d, } 20 { target aarch64_big_endian } } } */
/* 1 for each 16-bit type.  */
/* { dg-final { scan-assembler-times {\tld1rqh\tz[0-9]\.h, } 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #99\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #11\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #17\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #80\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #63\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #37\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #24\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #81\n} 2 } } */
/* The 32-bit types need:

      ZIP1 ZIP1 (2 ZIP2s optimized away)
      ZIP1 ZIP2

   and the 64-bit types need:

      ZIP1 ZIP1 ZIP1 ZIP1 (4 ZIP2s optimized away)
      ZIP1 ZIP2 ZIP1 ZIP2
      ZIP1 ZIP2 ZIP1 ZIP2.  */
/* { dg-final { scan-assembler-times {\tzip1\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 33 } } */
/* { dg-final { scan-assembler-times {\tzip2\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 15 } } */

/* The loop should be fully-masked.  The 32-bit types need two loads
   and stores each and the 64-bit types need four.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tst1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 3 } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 6 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 6 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 12 } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 12 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 12 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 24 } } */
/* { dg-final { scan-assembler-not {\tldr} } } */
/* { dg-final { scan-assembler-not {\tstr} } } */

/* { dg-final { scan-assembler-not {\tuqdec[bh]\t} } } */
/* We use UQDECW instead of UQDECD ..., MUL #2.  */
/* { dg-final { scan-assembler-times {\tuqdecw\t} 6 } } */
/* { dg-final { scan-assembler-times {\tuqdecd\t} 6 } } */
