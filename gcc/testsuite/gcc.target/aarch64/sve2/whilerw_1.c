/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

#define TEST_LOOP(TYPE)				\
  TYPE						\
  test_##TYPE (TYPE *dst, TYPE *src, int n)	\
  {						\
    TYPE res = 0;				\
    for (int i = 0; i < n; ++i)			\
      {						\
	dst[i] += 1;				\
	res += src[i];				\
      }						\
    return res;					\
  }

TEST_LOOP (int8_t);
TEST_LOOP (int16_t);
TEST_LOOP (int32_t);
TEST_LOOP (int64_t);

/* { dg-final { scan-assembler-times {\twhilerw\t} 4 } } */
/* { dg-final { scan-assembler-times {\twhilerw\tp[0-9]+\.b, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilerw\tp[0-9]+\.h, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilerw\tp[0-9]+\.s, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilerw\tp[0-9]+\.d, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-not {\twhilewr\t} } } */
