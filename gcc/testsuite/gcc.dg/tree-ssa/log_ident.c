/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */

extern void link_error(void);

#define T(TYPE, C_TY, FNAME)					\
  void f_##FNAME##_1 (TYPE a)					\
  {								\
    TYPE t1 = 1.0##C_TY / a;					\
    TYPE t2 = __builtin_##FNAME (t1);				\
    TYPE t3 = __builtin_##FNAME (a);				\
    TYPE t4 = -t3;						\
    if (t2 != t4)						\
      link_error ();						\
  }								\
  void f_##FNAME##_2 (TYPE a)					\
  {								\
    TYPE t1 = 2.0##C_TY / a;					\
    TYPE t2 = __builtin_##FNAME (t1);				\
    TYPE t3 = __builtin_##FNAME (2.0);				\
    TYPE t4 = __builtin_##FNAME (a);				\
    TYPE t5 = t3 - t4;						\
    if (t2 != t5)						\
      link_error ();						\
  }								\
  void f_##FNAME##_3 (TYPE a, TYPE b)				\
  {								\
    TYPE t1 = __builtin_##FNAME (a);				\
    TYPE t2 = __builtin_##FNAME (b);				\
    TYPE t3 = t1 + t2;						\
    TYPE t4 = a * b;						\
    TYPE t5 = __builtin_##FNAME (t4);				\
    if (t3 != t5)						\
      link_error ();						\
  }								\
  void f_##FNAME##_4 (TYPE a, TYPE b)				\
  {								\
    TYPE t1 = __builtin_##FNAME (a);				\
    TYPE t2 = __builtin_##FNAME (b);				\
    TYPE t3 = t1 - t2;						\
    TYPE t4 = a / b;						\
    TYPE t5 = __builtin_##FNAME (t4);				\
    if (t3 != t5)						\
      link_error ();						\
  }

#define TEST_LOGS(TYPE, C_TY, F_TY)				\
  T (TYPE, C_TY, log##F_TY)					\
  T (TYPE, C_TY, log2##F_TY)					\
  T (TYPE, C_TY, log10##F_TY)


TEST_LOGS (double, , )
TEST_LOGS (float, f, f)
TEST_LOGS (long double, L, l)

/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
