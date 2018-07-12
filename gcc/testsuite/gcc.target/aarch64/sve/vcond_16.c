/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define N 119

#define DEF_LOOP(INV, TYPE, CMPTYPE, SUFFIX)			\
  void __attribute__ ((noipa))					\
  f_##INV##_##SUFFIX (TYPE *restrict a, TYPE *restrict b,	\
		      TYPE *restrict c, TYPE *restrict d,	\
		      CMPTYPE *restrict cond)			\
  {								\
    for (int i = 0; i < N; ++i)					\
      {								\
	TYPE mb = (INV & 1 ? -b[i] : b[i]);			\
	TYPE mc = c[i];						\
	TYPE md = (INV & 2 ? -d[i] : d[i]);			\
	TYPE fma = __builtin_fma##SUFFIX (mb, mc, md);		\
	TYPE truev = (INV & 4 ? -fma : fma);			\
	a[i] = cond[i] < 10 ? truev : 10;			\
      }								\
  }

#define FOR_EACH_TYPE(T, INV)			\
  T (INV, _Float16, short, f16)			\
  T (INV, float, float, f32)			\
  T (INV, double, double, f64)

#define FOR_EACH_INV(T)				\
  FOR_EACH_TYPE (T, 0)				\
  FOR_EACH_TYPE (T, 1)				\
  FOR_EACH_TYPE (T, 2)				\
  FOR_EACH_TYPE (T, 3)				\
  FOR_EACH_TYPE (T, 4)				\
  FOR_EACH_TYPE (T, 5)				\
  FOR_EACH_TYPE (T, 6)				\
  FOR_EACH_TYPE (T, 7)

FOR_EACH_INV (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tsel\t} 24 } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.., z[0-9]+} } } */

/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+\.d,} 2 } } */

/* { dg-final { scan-assembler-times {\tfmls\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tfmls\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tfmls\tz[0-9]+\.d,} 2 } } */

/* { dg-final { scan-assembler-times {\tfnmla\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmla\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmla\tz[0-9]+\.d,} 2 } } */

/* { dg-final { scan-assembler-times {\tfnmls\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmls\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmls\tz[0-9]+\.d,} 2 } } */
