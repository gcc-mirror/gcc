/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, ABS)				\
  void __attribute__ ((noinline, noclone))		\
  test_##TYPE (TYPE *__restrict r, TYPE *__restrict a,	\
	       TYPE *__restrict b, TYPE *__restrict c,	\
	       int n)					\
  {							\
    for (int i = 0; i < n; ++i)				\
      r[i] = a[i] < 20 ? ABS (b[i] - c[i]) : 8.0;	\
  }

#define TEST_ALL(T) \
  T (_Float16, __builtin_fabsf16) \
  T (float, __builtin_fabsf) \
  T (double, __builtin_fabs)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-times {\tsel\t} 3 } } */
