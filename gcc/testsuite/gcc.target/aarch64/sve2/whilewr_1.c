/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

#define TEST_LOOP(TYPE)						\
  void								\
  test_##TYPE (TYPE *dst, TYPE *src1, TYPE *src2, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      dst[i] = src1[i] + src2[i];				\
  }

TEST_LOOP (int8_t);
TEST_LOOP (int16_t);
TEST_LOOP (int32_t);
TEST_LOOP (int64_t);

/* { dg-final { scan-assembler-times {\twhilewr\t} 8 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.b, x1, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.b, x2, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.h, x1, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.h, x2, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.s, x1, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.s, x2, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.d, x1, x0\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.d, x2, x0\n} 1 } } */
/* { dg-final { scan-assembler-not {\twhilerw\t} } } */
