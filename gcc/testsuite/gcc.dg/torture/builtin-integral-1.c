/* Copyright (C) 2004  Free Software Foundation.

   Verify that integral FP expressions are optimized.

   Written by Kaveh Ghazi, 2004-03-16.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

#define PROTOTYPE1(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);

PROTOTYPE1(fabs)

void test(int i1, int i2)
{
  /* Test that the various FP truncation builtins detect integral
     arguments.  */
#define CHECK_FN(MATHFN) \
 PROTOTYPE1 (MATHFN) \
 extern void link_failure_##MATHFN(void); \
 extern void link_failure_##MATHFN##f(void); \
 extern void link_failure_##MATHFN##l(void); \
 if (MATHFN(i1) != i1) link_failure_##MATHFN(); \
 if (MATHFN##f(i1) != i1) link_failure_##MATHFN##f(); \
 if (MATHFN##l(i1) != i1) link_failure_##MATHFN##l(); \

  CHECK_FN(ceil);
  CHECK_FN(floor);
  CHECK_FN(nearbyint);
  CHECK_FN(rint);
  CHECK_FN(round);
  CHECK_FN(trunc);

  /* Check that various other integral expressions are detected.  */
#define CHECK_EXPR(EXPR,NAME) \
 extern void link_failure_##NAME(void); \
 if (ceill(EXPR) != (EXPR)) link_failure_##NAME(); \

  CHECK_EXPR (5.0, REAL_CST);
  CHECK_EXPR (5.0F, REAL_CSTf);
  CHECK_EXPR (5.0L, REAL_CSTl);
  CHECK_EXPR ((double)i1, FLOAT_EXPR);
  CHECK_EXPR ((float)i1, FLOAT_EXPRf);
  CHECK_EXPR ((long double)i1, FLOAT_EXPRl);
  CHECK_EXPR (fabs(i1), ABS_EXPR);
  CHECK_EXPR (fabsf(i1), ABS_EXPRf);
  CHECK_EXPR (fabsl(i1), ABS_EXPRl);
  CHECK_EXPR (((void)i1,(double)i2), COMPOUND_EXPR);
  CHECK_EXPR ((double)i1+i2, PLUS_EXPR);
  CHECK_EXPR ((double)i1-i2, MINUS_EXPR);
  CHECK_EXPR ((double)i1*i2, MULT_EXPR);
}

int main (void)
{
  return 0;
}
