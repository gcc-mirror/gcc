/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-tree-loop-distribute-patterns" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

#define TEST_LOOP(TYPE)						\
  void								\
  test_##TYPE (TYPE *dst1, TYPE *dst2, TYPE *dst3, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
        dst1[i] = 1;						\
        dst2[i] = 2;						\
        dst3[i] = 3;						\
      }								\
   }

TEST_LOOP (int8_t);
TEST_LOOP (int16_t);
TEST_LOOP (int32_t);
TEST_LOOP (int64_t);

/* { dg-final { scan-assembler-times {\twhilewr\t} 12 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.b, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.b, x0, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.b, x1, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.h, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.h, x0, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.h, x1, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.s, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.s, x0, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.s, x1, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.d, x0, x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.d, x0, x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilewr\tp[0-9]+\.d, x1, x2\n} 1 } } */
/* { dg-final { scan-assembler-not {\twhilerw\t} } } */
