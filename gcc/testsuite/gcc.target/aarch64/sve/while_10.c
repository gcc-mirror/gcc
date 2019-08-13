/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=512" } */

#include <stdint.h>

#define ADD_LOOP(TYPE, COUNT)			\
  TYPE __attribute__ ((noinline, noclone))	\
  vec_while_##TYPE (TYPE *restrict a)		\
  {						\
    for (int i = 0; i < COUNT; ++i)	       	\
      a[i] += 1;				\
  }

#define TEST_ALL(T)				\
  T (int8_t, 63)				\
  T (int16_t, 30)				\
  T (int32_t, 15)				\
  T (int64_t, 6)

TEST_ALL (ADD_LOOP)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, mul3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.h, mul3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s, mul3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d, vl6\n} 1 } } */
