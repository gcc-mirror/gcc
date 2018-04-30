/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define N 2000

#define TEST_LOOP(NAME, TYPE)					\
  void __attribute__ ((noinline, noclone))			\
  NAME (TYPE *restrict dest, TYPE *restrict src)		\
  {								\
    for (int i = 0; i < N; ++i)					\
      dest[i] += src[i * 3];					\
  }

#define TEST(NAME) \
  TEST_LOOP (NAME##_i8, int8_t) \
  TEST_LOOP (NAME##_i16, uint16_t) \
  TEST_LOOP (NAME##_f32, float) \
  TEST_LOOP (NAME##_f64, double)

TEST (test)

/* Check the vectorized loop.  */
/* { dg-final { scan-assembler-times {\tld1b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld3b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld3h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld3w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\t} 1 } } */
/* { dg-final { scan-assembler-times {\tld3d\t} 1 } } */
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

/* The only branches should be in the vectorized loop.  */
/* { dg-final { scan-assembler-times {\tb[a-z]+\t} 4 } } */
