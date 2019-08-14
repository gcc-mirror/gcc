/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define abs(A) ((A) < 0 ? -(A) : (A))
#define neg(A) (-(A))

#define DEF_LOOP(TYPE, OP)					\
  void __attribute__ ((noipa))					\
  test_##TYPE##_##OP (TYPE *__restrict r, TYPE *__restrict a,	\
		      TYPE *__restrict b,			\
		      TYPE *__restrict pred, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = pred[i] ? OP (a[i]) : b[i];			\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, abs) \
  T (TYPE, neg)

#define TEST_ALL(T) \
  TEST_TYPE (T, int8_t) \
  TEST_TYPE (T, int16_t) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, int64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
