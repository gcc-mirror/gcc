/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define DEF_LOOP(TYPE, ABS, NAME, OP)			\
  void							\
  test_##TYPE##_##NAME (TYPE *restrict r,		\
			TYPE *restrict a,		\
			TYPE *restrict b, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      r[i] = ABS (a[i]) OP ABS (b[i]) ? 1.0 : 0.0;	\
  }

#define TEST_TYPE(T, TYPE, ABS)			\
  T (TYPE, ABS, lt, <)				\
  T (TYPE, ABS, le, <=)				\
  T (TYPE, ABS, ge, >=)				\
  T (TYPE, ABS, gt, >)

#define TEST_ALL(T)				\
  TEST_TYPE (T, _Float16, __builtin_fabsf16)	\
  TEST_TYPE (T, float, __builtin_fabsf)		\
  TEST_TYPE (T, double, __builtin_fabs)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tfac[lg]t\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfac[lg]e\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfac[lg]t\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfac[lg]e\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfac[lg]t\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfac[lg]e\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
