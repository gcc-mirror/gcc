/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized -fexcess-precision=16" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-require-effective-target c99_runtime } */

extern void link_error (void);

#define POW1OVER(TYPE1, TYPE2, CTY, TY)			\
  void							\
  pow1over_##TY (TYPE1 x, TYPE2 y)			\
  {							\
    TYPE1 t1 = 1.0##CTY / x;				\
    TYPE1 t2 = __builtin_pow##TY (t1, y);		\
    TYPE2 t3 = -y;					\
    TYPE1 t4 = __builtin_pow##TY (x, t3);		\
    if (t2 != t4)					\
      link_error ();					\
  }							\

#define POW0(TYPE1, TYPE2, CTY, TY)			\
  void							\
  pow0_##TY (TYPE2 x)					\
  {							\
    TYPE1 t1 = __builtin_pow##TY (0.0##CTY, x);		\
    if (t1 != 0.0##CTY)					\
      link_error ();					\
  }							\

#define TEST_ALL(TYPE1, TYPE2, CTY, TY)			\
  POW1OVER (TYPE1, TYPE2, CTY, TY)			\
  POW0 (TYPE1, TYPE2, CTY, TY)

TEST_ALL (double, double, , )
TEST_ALL (float, float, f, f)
TEST_ALL (_Float16, _Float16, f16, f16)
TEST_ALL (long double, long double, L, l)
TEST_ALL (double, int, , i)
TEST_ALL (float, int, f, if)
TEST_ALL (long double, int, L, il)

/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
