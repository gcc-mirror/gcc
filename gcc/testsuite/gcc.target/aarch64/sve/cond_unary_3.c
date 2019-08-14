/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define abs(A) ((A) < 0 ? -(A) : (A))
#define neg(A) (-(A))

#define DEF_LOOP(TYPE, OP)					\
  void __attribute__ ((noipa))					\
  test_##TYPE##_##OP (TYPE *__restrict r, TYPE *__restrict a,	\
		      TYPE *__restrict pred, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = pred[i] ? OP (a[i]) : 5;				\
  }

#define TEST_INT_TYPE(T, TYPE) \
  T (TYPE, abs) \
  T (TYPE, neg)

#define TEST_FLOAT_TYPE(T, TYPE, SUFFIX) \
  T (TYPE, __builtin_fabs##SUFFIX) \
  T (TYPE, neg)

#define TEST_ALL(T) \
  TEST_INT_TYPE (T, int8_t) \
  TEST_INT_TYPE (T, int16_t) \
  TEST_INT_TYPE (T, int32_t) \
  TEST_INT_TYPE (T, int64_t) \
  TEST_FLOAT_TYPE (T, _Float16, f16) \
  TEST_FLOAT_TYPE (T, float, f) \
  TEST_FLOAT_TYPE (T, double, )

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+, z[0-9]+\n} 14 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^\n]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
