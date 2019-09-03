/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in folding of frexp is correctly performed by the
   compiler.

   Origin: Kaveh R. Ghazi,  February 21, 2007.  */

/* { dg-do link } */
/* { dg-options "-fno-finite-math-only" { target sh*-*-* } } */
/* In order to fold algebraic exprs below, targets with "composite"
   floating point formats need -funsafe-math-optimizations.  */
/* { dg-require-effective-target inf } */
/* { dg-options "-funsafe-math-optimizations" { target powerpc*-*-* } } */

extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* We can only check the exponent when optimizing since we rely on
   other optimizations to propagate the value.  TRUE means an error
   occurred.  */
#ifdef __OPTIMIZE__
#define CKEXP(X,Y) X != Y
#else
#define CKEXP(X,Y) 0
#endif

/* Test that frexp(ARG,&i) == RES && i == EXP.  Check the sign in
   case we get -0.0.  */
#define TESTIT_FREXP(ARG,RES,EXP) do { \
  int i = 12345; \
  if (__builtin_frexpf(ARG##f,&i) != RES##f \
      || CKEXP(i,EXP) \
      || CKSGN_F(__builtin_frexpf(ARG##f,&i),RES##f)) \
    link_error(__LINE__); \
  i = 12345; \
  if (__builtin_frexp(ARG,&i) != RES \
      || CKEXP(i,EXP) \
      || CKSGN(__builtin_frexp(ARG,&i),RES)) \
    link_error(__LINE__); \
  i = 12345; \
  if (__builtin_frexpl(ARG##l,&i) != RES##l \
      || CKEXP(i,EXP) \
      || CKSGN_L(__builtin_frexpl(ARG##l,&i),RES##l)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNCRES(frexp(NEG FUNCARG(ARGARG),&i)) is false.  Check
   the sign as well.  Ensure side-effects are evaluated in i.  */
#define TESTIT_FREXP2(NEG,FUNCARG,ARGARG,FUNCRES) do { \
  int i=5; \
  if (!__builtin_##FUNCRES##f(__builtin_frexpf(NEG __builtin_##FUNCARG##f(ARGARG),&i)) \
      || CKSGN_F(__builtin_frexpf(NEG __builtin_##FUNCARG##f(ARGARG),(i++,&i)), NEG __builtin_##FUNCARG##f(ARGARG)) \
      || CKEXP(i,6)) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES(__builtin_frexp(NEG __builtin_##FUNCARG(ARGARG),&i)) \
      || CKSGN(__builtin_frexp(NEG __builtin_##FUNCARG(ARGARG),(i++,&i)), NEG __builtin_##FUNCARG(ARGARG)) \
      || CKEXP(i,7)) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES##l(__builtin_frexpl(NEG __builtin_##FUNCARG##l(ARGARG),&i)) \
      || CKSGN_L(__builtin_frexpl(NEG __builtin_##FUNCARG##l(ARGARG),(i++,&i)), NEG __builtin_##FUNCARG##l(ARGARG)) \
      || CKEXP(i,8)) \
    link_error(__LINE__); \
  } while (0)

void __attribute__ ((__noinline__))
foo(void)
{
  /* Test that frexp(ARG1,&i) -> ARG2 && i == ARG3.  */
  TESTIT_FREXP (-0x1p40, -0.5, 41);
  TESTIT_FREXP (-0x1p30, -0.5, 31);
  TESTIT_FREXP (-0x1p20, -0.5, 21);
  TESTIT_FREXP (-0x1p10, -0.5, 11);
  TESTIT_FREXP (-0x1p5, -0.5, 6);
  TESTIT_FREXP (-100/3.0, -100/192.0, 6);
  TESTIT_FREXP (-1.5, -0.75, 1);
  TESTIT_FREXP (-1.0, -0.5, 1);
  TESTIT_FREXP (-1/3.0, -2/3.0, -1);
  TESTIT_FREXP (-1/9.0, -8/9.0, -3);
  TESTIT_FREXP (-0x1p-5, -0.5, -4);
  TESTIT_FREXP (-0x1p-10, -0.5, -9);
  TESTIT_FREXP (-0x1p-20, -0.5, -19);
  TESTIT_FREXP (-0x1p-30, -0.5, -29);
  TESTIT_FREXP (-0x1p-40, -0.5, -39);
  TESTIT_FREXP (-0.0, -0.0, 0);
  TESTIT_FREXP (0.0, 0.0, 0);
  TESTIT_FREXP (0x1p-40, 0.5, -39);
  TESTIT_FREXP (0x1p-30, 0.5, -29);
  TESTIT_FREXP (0x1p-20, 0.5, -19);
  TESTIT_FREXP (0x1p-10, 0.5, -9);
  TESTIT_FREXP (0x1p-5, 0.5, -4);
  TESTIT_FREXP (1/9.0, 8/9.0, -3);
  TESTIT_FREXP (1/3.0, 2/3.0, -1);
  TESTIT_FREXP (1.0, 0.5, 1);
  TESTIT_FREXP (1.5, 0.75, 1);
  TESTIT_FREXP (100/3.0, 100/192.0, 6);
  TESTIT_FREXP (0x1p5, 0.5, 6);
  TESTIT_FREXP (0x1p10, 0.5, 11);
  TESTIT_FREXP (0x1p20, 0.5, 21);
  TESTIT_FREXP (0x1p30, 0.5, 31);
  TESTIT_FREXP (0x1p40, 0.5, 41);

  /* Test for frexp(+-Inf,&i) -> +-Inf and frexp(+-NaN,&i) -> +-NaN.
     Exponent is left unspecified, but we test for side-effects.  */
  TESTIT_FREXP2 ( ,inf, , isinf);
  TESTIT_FREXP2 (- ,inf, , isinf);
  TESTIT_FREXP2 ( ,nan, "", isnan);
  TESTIT_FREXP2 (- ,nan, "", isnan);
}

int main()
{
  foo ();
  
  return 0;
}
