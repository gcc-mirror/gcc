/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable -ffast-math" } */

#include <stdint.h>

#define VEC_PERM(TYPE)						\
void __attribute__ ((noinline, noclone))			\
vec_slp_##TYPE (TYPE *restrict a, TYPE *restrict b, int n)	\
{								\
  TYPE x0 = b[0];						\
  TYPE x1 = b[1];						\
  TYPE x2 = b[2];						\
  TYPE x3 = b[3];						\
  for (int i = 0; i < n; ++i)					\
    {								\
      x0 += a[i * 4];						\
      x1 += a[i * 4 + 1];					\
      x2 += a[i * 4 + 2];					\
      x3 += a[i * 4 + 3];					\
    }								\
  b[0] = x0;							\
  b[1] = x1;							\
  b[2] = x2;							\
  b[3] = x3;							\
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

/* We can't use SLP for the 64-bit loops, since the number of reduction
   results might be greater than the number of elements in the vector.
   Otherwise we have two loads per loop, one for the initial vector
   and one for the loop body.  */
/* ??? At present we don't treat the int8_t and int16_t loops as
   reductions.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 3 } } */
/* { dg-final { scan-assembler-times {\tld4d\t} 3 } } */
/* { dg-final { scan-assembler-not {\tld4b\t} } } */
/* { dg-final { scan-assembler-not {\tld4h\t} } } */
/* { dg-final { scan-assembler-not {\tld4w\t} } } */
/* { dg-final { scan-assembler-not {\tld1d\t} } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.b} 8 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.h} 8 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.b} 4 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.h} 4 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.s} 8 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.d} 8 } } */
/* { dg-final { scan-assembler-times {\tfaddv\th[0-9]+, p[0-7], z[0-9]+\.h} 4 } } */
/* { dg-final { scan-assembler-times {\tfaddv\ts[0-9]+, p[0-7], z[0-9]+\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tfaddv\td[0-9]+, p[0-7], z[0-9]+\.d} 4 } } */

/* Should be 4 and 6 respectively, if we used reductions for int8_t and
   int16_t.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b} 2 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h} 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d} 6 } } */

/* { dg-final { scan-assembler-not {\tuqdec} } } */
