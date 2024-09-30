/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#ifndef FACTOR
#define FACTOR 2
#endif

#define LOOP(TYPE)						\
  __attribute__ ((noipa))					\
  void								\
  test_##TYPE (TYPE *restrict dst, TYPE *restrict src,		\
	       int count)					\
  {								\
    for (int i = 0; i < count; ++i)				\
      dst[i] += src[i] * FACTOR;				\
  }

#define TEST_ALL(T) \
  T (int8_t) \
  T (int16_t) \
  T (int32_t) \
  T (int64_t) \
  T (uint8_t) \
  T (uint16_t) \
  T (uint32_t) \
  T (uint64_t)

TEST_ALL (LOOP)

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b,} 4 } } */
/* { dg-final { scan-assembler-not {\tadr\tz[0-9]+\.b,} } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h,} 4 } } */
/* { dg-final { scan-assembler-not {\tadr\tz[0-9]+\.h,} } } */

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-not {\tlsl\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.s, \[z[0-9]+\.s, z[0-9]+\.s, lsl 1\]} 2 } } */

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tlsl\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, lsl 1\]} 2 } } */
