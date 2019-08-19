/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE)				\
  void						\
  multi_loop_##TYPE (TYPE *x, TYPE val)		\
  {						\
    for (int i = 0; i < 9; ++i)			\
      x[i] += val;				\
    consumer (x);				\
    for (int i = 0; i < 9; ++i)			\
      x[i] += val;				\
    consumer (x);				\
    for (int i = 0; i < 9; ++i)			\
      x[i] += val;				\
    consumer (x);				\
  }

/* One iteration is enough.  */
TEST_LOOP (uint8_t);
/* Two iterations are enough.  We specialize the second two loops based
   on whether the first executes once or twice.  */
TEST_LOOP (uint16_t);
/* Three iterations are needed; ought to stay a loop.  */
TEST_LOOP (uint32_t);
/* Five iterations are needed; ought to stay a loop.  */
TEST_LOOP (uint64_t);

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-9]\.b} 3 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-9]\.h} 8 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-9]\.s} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-9]\.d} 6 } } */
/* { dg-final { scan-assembler-not {\tldr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tldr\tp[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tp[0-9]} } } */
