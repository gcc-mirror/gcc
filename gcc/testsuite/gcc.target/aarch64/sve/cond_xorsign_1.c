/* { dg-do compile } */
/* { dg-options "-O2" } */

#define xorsign(A, B, SUFFIX) ((A) * __builtin_copysign##SUFFIX (1.0, B))

#define DEF_LOOP(TYPE, SUFFIX)					\
  void __attribute__ ((noinline, noclone))			\
  test_##TYPE (TYPE *__restrict r, TYPE *__restrict a,		\
	       TYPE *__restrict b, TYPE *__restrict c,		\
	       int n)						\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = a[i] < 20 ? xorsign(b[i], c[i], SUFFIX) : b[i];	\
  }

#define TEST_ALL(T) \
  T (_Float16, f16) \
  T (float, f) \
  T (double, )

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, z[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tfmul} } } */
/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
