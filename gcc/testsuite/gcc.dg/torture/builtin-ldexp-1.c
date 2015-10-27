/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in folding of ldexp et al. is correctly performed
   by the compiler.

   Origin: Kaveh R. Ghazi,  February 17, 2007.  */

/* { dg-do link } */
/* { dg-options "-fno-finite-math-only" { target sh*-*-* } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* Test that FUNC(ARG1,ARG2) == RES.  Check the sign for -0.0.  */
#define TESTIT(FUNC,ARG1,ARG2,RES) do { \
  if (__builtin_##FUNC##f(ARG1##f,ARG2) != RES##f \
      || CKSGN_F(__builtin_##FUNC##f(ARG1##f,ARG2),RES##f)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1,ARG2) != RES \
      || CKSGN(__builtin_##FUNC(ARG1,ARG2),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1##l,ARG2) != RES##l \
      || CKSGN_L(__builtin_##FUNC##l(ARG1##l,ARG2),RES##l)) \
    link_error(__LINE__); \
  } while (0)

/* Test that (long)FUNC(ARG1,ARG2) == (long)RES.  The cast is
   necessary when RES is not a constant.  */
#define TESTIT2(FUNC,ARG1,ARG2,RES) do { \
  if ((long)__builtin_##FUNC##f(ARG1##f,ARG2) != (long)RES##f) \
    link_error(__LINE__); \
  if ((long)__builtin_##FUNC(ARG1,ARG2) != (long)RES) \
    link_error(__LINE__); \
  if ((long)__builtin_##FUNC##l(ARG1##l,ARG2) != (long)RES##l) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNCRES(FUNC(NEG FUNCARG(ARGARG),ARG2)) is false.  Check
   the sign as well.  */
#ifndef __SPU__
#define TESTIT3(FUNC,NEG,FUNCARG,ARGARG,ARG2,FUNCRES) do { \
  if (!__builtin_##FUNCRES##f(__builtin_##FUNC##f(NEG __builtin_##FUNCARG##f(ARGARG),ARG2)) \
      || CKSGN_F(__builtin_##FUNC##f(NEG __builtin_##FUNCARG##f(ARGARG),ARG2), NEG __builtin_##FUNCARG##f(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG),ARG2)) \
      || CKSGN(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG),ARG2), NEG __builtin_##FUNCARG(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES##l(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG),ARG2)) \
      || CKSGN_L(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG),ARG2), NEG __builtin_##FUNCARG##l(ARGARG))) \
    link_error(__LINE__); \
  } while (0)
#else
#define TESTIT3(FUNC,NEG,FUNCARG,ARGARG,ARG2,FUNCRES) do { \
  /* SPU single-precision floating point format does not support Inf or Nan.  */ \
  if (!__builtin_##FUNCRES(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG),ARG2)) \
      || CKSGN(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG),ARG2), NEG __builtin_##FUNCARG(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES##l(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG),ARG2)) \
      || CKSGN_L(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG),ARG2), NEG __builtin_##FUNCARG##l(ARGARG))) \
    link_error(__LINE__); \
  } while (0)
#endif

/* Using foo==MIN/MAX float values, test that FUNC(foo,EXP) == foo*exp2(EXP),
   and also that FUNC(foo,-EXP) == foo*exp2(-EXP).  */
#define TESTIT4(FUNC,EXP) do { \
  if (__builtin_##FUNC##f(__FLT_MIN__,EXP) != __FLT_MIN__*__builtin_exp2f(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##f(-__FLT_MIN__,EXP) != -__FLT_MIN__*__builtin_exp2f(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(__DBL_MIN__,EXP) != __DBL_MIN__*__builtin_exp2(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(-__DBL_MIN__,EXP) != -__DBL_MIN__*__builtin_exp2(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(__LDBL_MIN__,EXP) != __LDBL_MIN__*__builtin_exp2l(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(-__LDBL_MIN__,EXP) != -__LDBL_MIN__*__builtin_exp2l(EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##f(__FLT_MAX__,-EXP) != __FLT_MAX__*__builtin_exp2f(-EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##f(-__FLT_MAX__,-EXP) != -__FLT_MAX__*__builtin_exp2f(-EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(__DBL_MAX__,-EXP) != __DBL_MAX__*__builtin_exp2(-EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(-__DBL_MAX__,-EXP) != -__DBL_MAX__*__builtin_exp2(-EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(__LDBL_MAX__,-EXP) != __LDBL_MAX__*__builtin_exp2l(-EXP)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(-__LDBL_MAX__,-EXP) != -__LDBL_MAX__*__builtin_exp2l(-EXP)) \
    link_error(__LINE__); \
  } while (0)

void __attribute__ ((__noinline__))
foo(float xf, double x, long double xl, int i, long l)
{
  /* f(0.0, i) -> 0.0 and f(-0.0, i) -> -0.0.  */
  TESTIT (ldexp, 0.0, i, 0.0);
  TESTIT (ldexp, -0.0, i, -0.0);
  TESTIT (scalbn, 0.0, i, 0.0);
  TESTIT (scalbn, -0.0, i, -0.0);
  TESTIT (scalbln, 0.0, l, 0.0);
  TESTIT (scalbln, -0.0, l, -0.0);
  
  /* f(x,0) -> x.  */
  TESTIT2 (ldexp, x, 0, x);
  TESTIT2 (scalbn, x, 0, x);
  TESTIT2 (scalbln, x, 0, x);

  /* f(Inf,i) -> Inf and f(NaN,i) -> NaN.  */
  TESTIT3 (ldexp, , inf, , i, isinf);
  TESTIT3 (ldexp, -, inf, , i, isinf);
  TESTIT3 (ldexp, , nan, "", i, isnan);
  TESTIT3 (ldexp, -, nan, "", i, isnan);

  TESTIT3 (scalbn, , inf, , i, isinf);
  TESTIT3 (scalbn, -, inf, , i, isinf);
  TESTIT3 (scalbn, , nan, "", i, isnan);
  TESTIT3 (scalbn, -, nan, "", i, isnan);

  TESTIT3 (scalbln, , inf, , i, isinf);
  TESTIT3 (scalbln, -, inf, , i, isinf);
  TESTIT3 (scalbln, , nan, "", i, isnan);
  TESTIT3 (scalbln, -, nan, "", i, isnan);

  /* Evaluate when both arguments are constant.  */
  TESTIT (ldexp, 5.0, 3, 40.0);
  TESTIT (ldexp, -5.0, 3, -40.0);
  TESTIT (ldexp, 5.0, -3, 0.625);
  TESTIT (ldexp, -5.0, -3, -0.625);

  TESTIT (ldexp, 1000.0, 5, 32000.0);
  TESTIT (ldexp, -1000.0, 5, -32000.0);
  TESTIT (ldexp, 1000.0, -5, 31.25);
  TESTIT (ldexp, -1000.0, -5, -31.25);

  /* f(x,N) -> x*exp2(N), using MIN/MAX constants for x and constant N.  */
  TESTIT4 (ldexp, 1);
  TESTIT4 (ldexp, 2);
  TESTIT4 (ldexp, 3);
  TESTIT4 (ldexp, 5);
  TESTIT4 (ldexp, 9);
  TESTIT4 (ldexp, 10);
  TESTIT4 (ldexp, 12);
  TESTIT4 (ldexp, 18);
  TESTIT4 (ldexp, 25);
  TESTIT4 (ldexp, 50);
  TESTIT4 (ldexp, 75);
  TESTIT4 (ldexp, 100);
  TESTIT4 (ldexp, 123);

  /* These are folded when float radix is two.  */
#if __FLT_RADIX__ == 2
  TESTIT (scalbn, 5.0, 3, 40.0);
  TESTIT (scalbn, -5.0, 3, -40.0);
  TESTIT (scalbn, 5.0, -3, 0.625);
  TESTIT (scalbn, -5.0, -3, -0.625);

  TESTIT (scalbn, 1000.0, 5, 32000.0);
  TESTIT (scalbn, -1000.0, 5, -32000.0);
  TESTIT (scalbn, 1000.0, -5, 31.25);
  TESTIT (scalbn, -1000.0, -5, -31.25);

  TESTIT4 (scalbn, 1);
  TESTIT4 (scalbn, 2);
  TESTIT4 (scalbn, 3);
  TESTIT4 (scalbn, 5);
  TESTIT4 (scalbn, 9);
  TESTIT4 (scalbn, 10);
  TESTIT4 (scalbn, 12);
  TESTIT4 (scalbn, 18);
  TESTIT4 (scalbn, 25);
  TESTIT4 (scalbn, 50);
  TESTIT4 (scalbn, 75);
  TESTIT4 (scalbn, 100);
  TESTIT4 (scalbn, 123);

  TESTIT (scalbln, 5.0, 3, 40.0);
  TESTIT (scalbln, -5.0, 3, -40.0);
  TESTIT (scalbln, 5.0, -3, 0.625);
  TESTIT (scalbln, -5.0, -3, -0.625);

  TESTIT (scalbln, 1000.0, 5, 32000.0);
  TESTIT (scalbln, -1000.0, 5, -32000.0);
  TESTIT (scalbln, 1000.0, -5, 31.25);
  TESTIT (scalbln, -1000.0, -5, -31.25);

  TESTIT4 (scalbln, 1);
  TESTIT4 (scalbln, 2);
  TESTIT4 (scalbln, 3);
  TESTIT4 (scalbln, 5);
  TESTIT4 (scalbln, 9);
  TESTIT4 (scalbln, 10);
  TESTIT4 (scalbln, 12);
  TESTIT4 (scalbln, 18);
  TESTIT4 (scalbln, 25);
  TESTIT4 (scalbln, 50);
  TESTIT4 (scalbln, 75);
  TESTIT4 (scalbln, 100);
  TESTIT4 (scalbln, 123);
#endif
}

int main()
{
  foo (0, 0, 0, 0, 0);
  
  return 0;
}
