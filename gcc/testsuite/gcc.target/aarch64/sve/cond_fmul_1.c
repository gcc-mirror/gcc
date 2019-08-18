/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, PRED_TYPE, NAME, CONST)		\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict x,		\
			TYPE *__restrict y,		\
			PRED_TYPE *__restrict pred,	\
			int n)				\
  {							\
    for (int i = 0; i < n; ++i)				\
      x[i] = pred[i] != 1 ? y[i] * (TYPE) CONST : y[i];	\
  }

#define TEST_TYPE(T, TYPE, PRED_TYPE) \
  T (TYPE, PRED_TYPE, half, 0.5) \
  T (TYPE, PRED_TYPE, two, 2.0) \
  T (TYPE, PRED_TYPE, four, 4.0)

#define TEST_ALL(T) \
  TEST_TYPE (T, _Float16, int16_t) \
  TEST_TYPE (T, float, int32_t) \
  TEST_TYPE (T, double, int64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0\.5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0\.5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0\.5\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #2\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #2\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.h, #4\.0} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #4\.0} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #4\.0} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
