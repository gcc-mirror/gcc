/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE, VAL)			\
  void						\
  multi_loop_##TYPE (TYPE *x, int n)		\
  {						\
    for (int k = 0; k < 4; ++k)			\
      {						\
	for (int j = 0; j < n; ++j)		\
	  {					\
	    for (int i = 0; i < 100; ++i)	\
	      x[i] += VAL + i;			\
	    asm volatile ("");			\
	  }					\
	for (int j = 0; j < n; ++j)		\
	  consumer (x);				\
	for (int j = 0; j < n; ++j)		\
	  {					\
	    for (int i = 0; i < 100; ++i)	\
	      x[i] += VAL + i;			\
	    asm volatile ("");			\
	  }					\
	consumer (x);				\
	for (int i = 0; i < 100; ++i)		\
	  x[i] += VAL + i;			\
	consumer (x);				\
      }						\
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
