/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

void consumer (void *);

#define TEST_LOOP(TYPE, VAL)						\
  void									\
  multi_loop_##TYPE (TYPE *x1, TYPE *x2, TYPE *x3, TYPE *x4, int which) \
  {									\
    if (which)								\
      {									\
	for (int i = 0; i < 7; ++i)					\
	  x1[i] += VAL;							\
	consumer (x1);							\
	for (int i = 0; i < 7; ++i)					\
	  x2[i] -= VAL;							\
	consumer (x2);							\
      }									\
    else								\
      {									\
	for (int i = 0; i < 7; ++i)					\
	  x3[i] &= VAL;							\
	consumer (x3);							\
      }									\
    for (int i = 0; i < 7; ++i)						\
      x4[i] |= VAL;							\
    consumer (x4);							\
  }

TEST_LOOP (uint8_t, 0x12);
TEST_LOOP (uint16_t, 0x1234);
TEST_LOOP (uint32_t, 0x12345);
TEST_LOOP (uint64_t, 0x123456);

/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.b,} } } */
/* { dg-final { scan-assembler {\tld1h\tz[0-9]+\.h,} } } */
/* { dg-final { scan-assembler {\tld1w\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler {\tld1d\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tldr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
/* { dg-final { scan-assembler-not {\tldr\tp[0-9]} } } */
/* { dg-final { scan-assembler-not {\tstr\tp[0-9]} } } */
