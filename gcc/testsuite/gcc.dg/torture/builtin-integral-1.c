/* Copyright (C) 2004  Free Software Foundation.

   Verify that integral FP expressions are optimized.

   Written by Kaveh Ghazi, 2004-03-16.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

#define PROTOTYPE1(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);
#define PROTOTYPE1_RET(FN, RET) extern RET FN(double); extern RET FN##f(float); \
  extern RET FN##l(long double);
#define PROTOTYPE_LINK_FAILURE(FN) extern void link_failure_##FN(void); \
 extern void link_failure_##FN##f(void); \
 extern void link_failure_##FN##l(void); \

PROTOTYPE1(fabs)
PROTOTYPE1(ceil)
PROTOTYPE1(floor)
PROTOTYPE1(nearbyint)
PROTOTYPE1(rint)
PROTOTYPE1(round)
PROTOTYPE1(trunc)
PROTOTYPE1_RET(lround, long)
PROTOTYPE1_RET(llround, long long)
PROTOTYPE1_RET(lrint, long)
PROTOTYPE1_RET(llrint, long long)

/* Test that the various FP truncation builtins detect integral
   arguments.  */
#define CHECK_FN(MATHFN) \
 PROTOTYPE_LINK_FAILURE(MATHFN); \
 if (MATHFN(i1) != i1) link_failure_##MATHFN(); \
 if (MATHFN##f(i1) != i1) link_failure_##MATHFN##f(); \
 if (MATHFN##l(i1) != i1) link_failure_##MATHFN##l();

#define CHECK_FN_RET(MATHFN, RET) \
 PROTOTYPE_LINK_FAILURE(MATHFN); \
 if (MATHFN(i1) != (RET)(double)i1) link_failure_##MATHFN(); \
 if (MATHFN##f(i1) != (RET)(float)i1) link_failure_##MATHFN##f(); \
 if (MATHFN##l(i1) != (RET)(long double)i1) link_failure_##MATHFN##l();

  /* Check that various other integral expressions are detected.  */
#define CHECK_EXPR(EXPR,NAME) \
 extern void link_failure_FP_##NAME(void); \
 extern void link_failure_fixed_##NAME(void); \
 if (ceill(EXPR) != (EXPR)) link_failure_FP_##NAME(); \
 if (lroundl(EXPR) != (long)(long double)(EXPR)) link_failure_fixed_##NAME();

void __attribute__ ((__noinline__)) test (int i1, int i2)
{
  CHECK_FN(ceil);
  CHECK_FN(floor);
  CHECK_FN(nearbyint);
  CHECK_FN(rint);
  CHECK_FN(round);
  CHECK_FN(trunc);
  CHECK_FN_RET(lround, long);
  CHECK_FN_RET(llround, long long);
  CHECK_FN_RET(lrint, long);
  CHECK_FN_RET(llrint, long long);

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
  test (1, 2);
  return 0;
}
