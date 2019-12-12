/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint.h>

#define ADD_LOOP(TYPE)				\
  TYPE __attribute__ ((noinline, noclone))	\
  vec_while_##TYPE (TYPE *restrict a)		\
  {						\
    for (int i = 0; i < 8; ++i)			\
      a[i] += 1;				\
  }

#define TEST_ALL(T)				\
  T (int8_t)					\
  T (int16_t)					\
  T (int32_t)					\
  T (int64_t)

TEST_ALL (ADD_LOOP)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.h, vl8\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d,} 2 } } */
