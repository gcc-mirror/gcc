/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE) \
  void								\
  multi_loop_##TYPE (TYPE *x, TYPE val1, TYPE val2, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	x[i * 2] += val1;					\
	x[i * 2 + 1] += val2;					\
      }								\
    consumer (x);						\
    for (int i = 0; i < n; ++i)					\
      {								\
	x[i * 2] += val1;					\
	x[i * 2 + 1] += val2;					\
      }								\
    consumer (x);						\
    for (int i = 0; i < n; ++i)					\
      {								\
	x[i * 2] += val1;					\
	x[i * 2 + 1] += val2;					\
      }								\
    consumer (x);						\
  }

/* One iteration is enough.  */
TEST_LOOP (uint8_t);
TEST_LOOP (uint16_t);
/* Two iterations are enough.  Complete unrolling makes sense
   even at -O2.  */
TEST_LOOP (uint32_t);
/* Four iterations are needed; ought to stay a loop.  */
TEST_LOOP (uint64_t);

/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.b} } } */
/* { dg-final { scan-assembler {\tld1h\tz[0-9]+\.h} } } */
/* { dg-final { scan-assembler {\tld1w\tz[0-9]+\.s} } } */
/* { dg-final { scan-assembler {\tld1d\tz[0-9]+\.d} } } */
/* { dg-final { scan-assembler-not {\tldr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tldr\tp[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tp[0-9]} } } */
