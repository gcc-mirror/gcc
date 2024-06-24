/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math" } */

#include <stdint.h>

#define DEF_LOOP(FLOAT_TYPE, INT_TYPE)				\
  void __attribute__ ((noipa))					\
  test_##INT_TYPE (INT_TYPE *__restrict r,			\
		   FLOAT_TYPE *__restrict a,			\
		   INT_TYPE *__restrict pred, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = pred[i] ? (INT_TYPE) a[i] : 0;			\
  }

#define TEST_ALL(T) \
  T (_Float16, int16_t) \
  T (_Float16, uint16_t) \
  T (float, int32_t) \
  T (float, uint32_t) \
  T (double, int64_t) \
  T (double, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzu\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzu\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.d, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzu\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/z,} 2 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/z,} 2 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.d, p[0-7]/z,} 2 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^\n]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
