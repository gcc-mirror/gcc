/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE)						\
  void __attribute__ ((noipa))					\
  test_##TYPE (TYPE *restrict r, TYPE *restrict a, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = !a[i];						\
  }

#define TEST_ALL(T)		\
  T (int8_t)			\
  T (int16_t)			\
  T (int32_t)			\
  T (int64_t)			\
  T (uint8_t)			\
  T (uint16_t)			\
  T (uint32_t)			\
  T (uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-not {\tsel\t} } } */
/* { dg-final { scan-assembler-times {\tcnot\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcnot\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcnot\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcnot\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 2 } } */
