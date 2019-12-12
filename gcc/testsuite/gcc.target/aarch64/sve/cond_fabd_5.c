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
      r[i] = a[i] < 20 ? ABS (b[i] - c[i]) : 0.0;	\
  }

#define TEST_ALL(T) \
  T (_Float16, __builtin_fabsf16) \
  T (float, __builtin_fabsf) \
  T (double, __builtin_fabs)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* Really we should be able to use MOVPRFX /Z here, but at the moment
   we're relying on combine to merge a SEL and an arithmetic operation,
   and the SEL doesn't allow zero operands.  */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/z, z[0-9]+\.h\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/z, z[0-9]+\.s\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.d, p[0-7]/z, z[0-9]+\.d\n} 1 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} { xfail *-*-* } } } */
