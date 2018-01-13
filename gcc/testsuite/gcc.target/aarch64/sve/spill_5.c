/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE, VAL)			\
  void						\
  multi_loop_##TYPE (TYPE *x)			\
  {						\
    for (int i = 0; i < 100; ++i)		\
      x[i] += VAL + i;				\
    consumer (x);				\
    for (int i = 0; i < 100; ++i)		\
      x[i] += VAL + i;				\
    consumer (x);				\
    for (int i = 0; i < 100; ++i)		\
      x[i] += VAL + i;				\
    consumer (x);				\
  }

TEST_LOOP (uint8_t, 3);
TEST_LOOP (uint16_t, 4);
TEST_LOOP (uint32_t, 5);
TEST_LOOP (uint64_t, 6);
TEST_LOOP (float, 2.5f);
TEST_LOOP (double, 3.5);

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\..,} 18 } } */
/* { dg-final { scan-assembler-not {\tldr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tldr\tp[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tp[0-9]} } } */
