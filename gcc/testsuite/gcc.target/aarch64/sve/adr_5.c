/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define LOOP(FACTOR)						\
  __attribute__ ((noipa))					\
  void								\
  test_##FACTOR (uint64_t *restrict dst,			\
		 uint64_t *restrict src, int count)		\
  {								\
    for (int i = 0; i < count; ++i)				\
      dst[i] += (src[i] & 0xffffffff) * FACTOR;			\
  }

#define TEST_ALL(T) T (1) T (2) T (4) T (8)

TEST_ALL (LOOP)

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tlsl\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tuxtw\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, uxtw\]} 1 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, uxtw 1\]} 1 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, uxtw 2\]} 1 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, uxtw 3\]} 1 } } */
