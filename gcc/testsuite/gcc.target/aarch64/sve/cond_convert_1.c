/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math" } */

#include <stdint.h>

#define DEF_LOOP(FLOAT_TYPE, INT_TYPE)				\
  void __attribute__ ((noipa))					\
  test_##INT_TYPE (FLOAT_TYPE *__restrict r,			\
		   INT_TYPE *__restrict a,			\
		   FLOAT_TYPE *__restrict b,			\
		   INT_TYPE *__restrict pred, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	FLOAT_TYPE bi = b[i];					\
	r[i] = pred[i] ? (FLOAT_TYPE) a[i] : bi;		\
      }								\
  }

#define TEST_ALL(T) \
  T (_Float16, int16_t) \
  T (_Float16, uint16_t) \
  T (float, int32_t) \
  T (float, uint32_t) \
  T (double, int64_t) \
  T (double, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.d, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
