/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_LOOP(NAME, TYPE)					\
  void __attribute__ ((noinline, noclone))			\
  NAME (TYPE *restrict dest, TYPE *restrict src, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      dest[i] += src[i * 2];					\
  }

#define TEST(NAME) \
  TEST_LOOP (NAME##_i8, int8_t) \
  TEST_LOOP (NAME##_i16, uint16_t) \
  TEST_LOOP (NAME##_f32, float) \
  TEST_LOOP (NAME##_f64, double)

TEST (test)

/* Check the vectorized loop.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld2b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld2h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld2w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld2d\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1d\t} 1 } } */

/* Check the scalar tail.  */
/* { dg-final { scan-assembler-times {\tldrb\tw} 2 } } */
/* { dg-final { scan-assembler-times {\tstrb\tw} 1 } } */
/* { dg-final { scan-assembler-times {\tldrh\tw} 2 } } */
/* { dg-final { scan-assembler-times {\tstrh\tw} 1 } } */
/* { dg-final { scan-assembler-times {\tldr\ts} 2 } } */
/* { dg-final { scan-assembler-times {\tstr\ts} 1 } } */
/* { dg-final { scan-assembler-times {\tldr\td} 2 } } */
/* { dg-final { scan-assembler-times {\tstr\td} 1 } } */

/* Each function should have three branches: one directly to the exit
   (n <= 0), one to the single scalar epilogue iteration (n == 1),
   and one branch-back for the vectorized loop.  */
/* { dg-final { scan-assembler-times {\tb[a-z]+\t} 12 } } */
