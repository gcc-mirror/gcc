/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE, VAL)			\
  void						\
  double_loop_##TYPE (TYPE *x)			\
  {						\
    for (int i = 0; i < 100; ++i)		\
      x[i] += VAL;				\
    consumer (x);				\
    for (int i = 0; i < 100; ++i)		\
      x[i] += VAL;				\
    consumer (x);				\
  }

TEST_LOOP (uint16_t, 511);
TEST_LOOP (uint32_t, 511);
TEST_LOOP (uint64_t, 511);

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #511\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, #511\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #511\n} 2 } } */
/* { dg-final { scan-assembler-not {\tldr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tldr\tp[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tp[0-9]} } } */
