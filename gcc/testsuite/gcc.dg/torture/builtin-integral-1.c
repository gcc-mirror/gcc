/* Copyright (C) 2004  Free Software Foundation.

   Verify that integral FP expressions are optimized.

   Written by Kaveh Ghazi, 2004-03-16.  */

/* { dg-do link } */
/* We need -ffinite-math-only so that we can fold "foo != foo", where
   foo is a floating point expression.  We need -fno-math-errno so
   that various math functions are marked const/pure and can be
   folded.  */
/* { dg-options "-ffinite-math-only -fno-math-errno" } */

extern int link_failure (int);

/* Test that the various FP truncation builtins detect integral
   arguments.  */
#define CHECK_FN(MATHFN) \
 if (__builtin_##MATHFN(i1) != i1) link_failure (__LINE__); \
 if (__builtin_##MATHFN##f(i1) != i1) link_failure (__LINE__); \
 if (__builtin_##MATHFN##l(i1) != i1) link_failure (__LINE__);

#define CHECK_FN_RET(MATHFN, RET) \
 if (__builtin_##MATHFN(i1) != (RET)(double)i1) link_failure (__LINE__); \
 if (__builtin_##MATHFN##f(i1) != (RET)(float)i1) link_failure (__LINE__); \
 if (__builtin_##MATHFN##l(i1) != (RET)(long double)i1) link_failure (__LINE__);

  /* Check that various other integral expressions are detected.  */
#define CHECK_EXPR(EXPR,NAME) \
 if (__builtin_ceill(EXPR) != (EXPR)) link_failure (__LINE__); \
 if (__builtin_lroundl(EXPR) != (long)(long double)(EXPR)) link_failure (__LINE__);

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
  CHECK_FN_RET(lceil, long);
  CHECK_FN_RET(llceil, long long);
  CHECK_FN_RET(lfloor, long);
  CHECK_FN_RET(llfloor, long long);

  CHECK_EXPR (5.0, REAL_CST);
  CHECK_EXPR (5.0F, REAL_CSTf);
  CHECK_EXPR (5.0L, REAL_CSTl);
  CHECK_EXPR ((double)i1, FLOAT_EXPR);
  CHECK_EXPR ((float)i1, FLOAT_EXPRf);
  CHECK_EXPR ((long double)i1, FLOAT_EXPRl);
  CHECK_EXPR (__builtin_fabs(i1), ABS_EXPR);
  CHECK_EXPR (__builtin_fabsf(i1), ABS_EXPRf);
  CHECK_EXPR (__builtin_fabsl(i1), ABS_EXPRl);
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
