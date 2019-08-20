/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#ifndef FN
#define FN(X) __builtin_fmax##X
#endif

#define DEF_LOOP(FN, TYPE, NAME, CONST)			\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict x,		\
			TYPE *__restrict y, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      x[i] = FN (y[i], CONST);				\
  }

#define TEST_TYPE(T, FN, TYPE) \
  T (FN, TYPE, zero, 0) \
  T (FN, TYPE, one, 1) \
  T (FN, TYPE, two, 2)

#define TEST_ALL(T) \
  TEST_TYPE (T, FN (f16), _Float16) \
  TEST_TYPE (T, FN (f32), float) \
  TEST_TYPE (T, FN (f64), double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #1\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.h, #2\.0} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #2\.0} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #2\.0} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
