/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in folding of logb, ilogb and significand is
   correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  February 22, 2007.  */

/* { dg-do link } */
/* { dg-options "-fno-finite-math-only" { target sh*-*-* } } */
/* In order to fold algebraic exprs below, targets with "composite"
   floating point formats need -funsafe-math-optimizations.  */
/* { dg-options "-funsafe-math-optimizations" { target mips*-*-irix6* powerpc*-*-* } } */

extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* Test that FUNC(ARG) == RES.  Check the sign in case we get -0.0.  */
#define TESTIT(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG##f) != RES##f \
      || CKSGN_F(__builtin_##FUNC##f(ARG##f),RES##f)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES \
      || CKSGN(__builtin_##FUNC(ARG),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG##l) != RES##l \
      || CKSGN_L(__builtin_##FUNC##l(ARG##l),RES##l)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(ARG) == RES.  RES is an int so it can't be -0.0.  */
#define TESTIT2(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG##f) != RES) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG##l) != RES) \
    link_error(__LINE__); \
  } while (0)

/* Test if FUNCRES(FUNC(NEG FUNCARG(ARGARG))) is false.  Check the
   sign as well.  */
#ifndef __SPU__
#define TESTIT3(FUNC,NEG,FUNCARG,ARGARG,FUNCRES,NEG2) do { \
  if (!__builtin_##FUNCRES##f(__builtin_##FUNC(NEG __builtin_##FUNCARG##f(ARGARG))) \
      || CKSGN_F(__builtin_##FUNC##f(NEG __builtin_##FUNCARG##f(ARGARG)), NEG2 __builtin_##FUNCARG##f(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG))) \
      || CKSGN(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG)), NEG2 __builtin_##FUNCARG(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES##l(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG))) \
      || CKSGN_L(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG)), NEG2 __builtin_##FUNCARG##l(ARGARG))) \
    link_error(__LINE__); \
  } while (0)
#else
#define TESTIT3(FUNC,NEG,FUNCARG,ARGARG,FUNCRES,NEG2) do { \
  /* SPU single-precision floating point format does not support Inf or Nan.  */ \
  if (!__builtin_##FUNCRES(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG))) \
      || CKSGN(__builtin_##FUNC(NEG __builtin_##FUNCARG(ARGARG)), NEG2 __builtin_##FUNCARG(ARGARG))) \
    link_error(__LINE__); \
  if (!__builtin_##FUNCRES##l(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG))) \
      || CKSGN_L(__builtin_##FUNC##l(NEG __builtin_##FUNCARG##l(ARGARG)), NEG2 __builtin_##FUNCARG##l(ARGARG))) \
    link_error(__LINE__); \
  } while (0)
#endif

void __attribute__ ((__noinline__))
foo(void)
{
  /* If radix == 2, test that logb(ARG2) -> ARG3.  */
#if __FLT_RADIX__ == 2
  TESTIT (logb, -0x1p40, 40.0);
  TESTIT (logb, -0x1p30, 30.0);
  TESTIT (logb, -0x1p20, 20.0);
  TESTIT (logb, -0x1p10, 10.0);
  TESTIT (logb, -0x1p5, 5.0);
  TESTIT (logb, -100/3.0, 5.0);
  TESTIT (logb, -2.0, 1.0);
  TESTIT (logb, -1.5, 0.0);
  TESTIT (logb, -1.0, 0.0);
  TESTIT (logb, -1/3.0, -2.0);
  TESTIT (logb, -1/9.0, -4.0);
  TESTIT (logb, -0x1p-5, -5.0);
  TESTIT (logb, -0x1p-10, -10.0);
  TESTIT (logb, -0x1p-20, -20.0);
  TESTIT (logb, -0x1p-30, -30.0);
  TESTIT (logb, -0x1p-40, -40.0);

  TESTIT (logb, 0x1p-40, -40.0);
  TESTIT (logb, 0x1p-30, -30.0);
  TESTIT (logb, 0x1p-20, -20.0);
  TESTIT (logb, 0x1p-10, -10.0);
  TESTIT (logb, 0x1p-5, -5.0);
  TESTIT (logb, 1/9.0, -4.0);
  TESTIT (logb, 1/3.0, -2.0);
  TESTIT (logb, 1.0, 0.0);
  TESTIT (logb, 1.5, 0.0);
  TESTIT (logb, 2.0, 1.0);
  TESTIT (logb, 100/3.0, 5.0);
  TESTIT (logb, 0x1p5, 5.0);
  TESTIT (logb, 0x1p10, 10.0);
  TESTIT (logb, 0x1p20, 20.0);
  TESTIT (logb, 0x1p30, 30.0);
  TESTIT (logb, 0x1p40, 40.0);
#endif

  /* If radix == 2, test that ilogb(ARG2) -> ARG3.  */
#if __FLT_RADIX__ == 2
  TESTIT2 (ilogb, -0x1p40, 40);
  TESTIT2 (ilogb, -0x1p30, 30);
  TESTIT2 (ilogb, -0x1p20, 20);
  TESTIT2 (ilogb, -0x1p10, 10);
  TESTIT2 (ilogb, -0x1p5, 5);
  TESTIT2 (ilogb, -100/3.0, 5);
  TESTIT2 (ilogb, -2.0, 1);
  TESTIT2 (ilogb, -1.5, 0);
  TESTIT2 (ilogb, -1.0, 0);
  TESTIT2 (ilogb, -1/3.0, -2);
  TESTIT2 (ilogb, -1/9.0, -4);
  TESTIT2 (ilogb, -0x1p-5, -5);
  TESTIT2 (ilogb, -0x1p-10, -10);
  TESTIT2 (ilogb, -0x1p-20, -20);
  TESTIT2 (ilogb, -0x1p-30, -30);
  TESTIT2 (ilogb, -0x1p-40, -40);

  TESTIT2 (ilogb, 0x1p-40, -40);
  TESTIT2 (ilogb, 0x1p-30, -30);
  TESTIT2 (ilogb, 0x1p-20, -20);
  TESTIT2 (ilogb, 0x1p-10, -10);
  TESTIT2 (ilogb, 0x1p-5, -5);
  TESTIT2 (ilogb, 1/9.0, -4);
  TESTIT2 (ilogb, 1/3.0, -2);
  TESTIT2 (ilogb, 1.0, 0);
  TESTIT2 (ilogb, 1.5, 0);
  TESTIT2 (ilogb, 2.0, 1);
  TESTIT2 (ilogb, 100/3.0, 5);
  TESTIT2 (ilogb, 0x1p5, 5);
  TESTIT2 (ilogb, 0x1p10, 10);
  TESTIT2 (ilogb, 0x1p20, 20);
  TESTIT2 (ilogb, 0x1p30, 30);
  TESTIT2 (ilogb, 0x1p40, 40);
#endif

  /* If radix == 2, test that significand(ARG2) -> ARG3.  Zero always
     folds regardless of the radix.  */
  TESTIT (significand, -0.0, -0.0);
  TESTIT (significand, 0.0, 0.0);

#if __FLT_RADIX__ == 2
  TESTIT (significand, -0x1p5, -1.0);
  TESTIT (significand, -100/3.0, -100/96.0);
  TESTIT (significand, -1.5, -1.5);
  TESTIT (significand, -1.0, -1.0);
  TESTIT (significand, -1/3.0, -4/3.0);
  TESTIT (significand, -1/9.0, -16/9.0);
  TESTIT (significand, -0x1p-5, -1.0);

  TESTIT (significand, 0x1p-5, 1.0);
  TESTIT (significand, 1/9.0, 16/9.0);
  TESTIT (significand, 1/3.0, 4/3.0);
  TESTIT (significand, 1.0, 1.0);
  TESTIT (significand, 1.5, 1.5);
  TESTIT (significand, 100/3.0, 100/96.0);
  TESTIT (significand, 0x1p5, 1.0);
#endif

  /* Test for f(+-Inf) -> +-Inf and f(+-NaN) -> +-NaN, regardless of
     the radix.  */
  TESTIT3 (logb, ,inf, , isinf, );
  TESTIT3 (logb, - ,inf, , isinf, );
  TESTIT3 (logb,  ,nan, "", isnan, );
  TESTIT3 (logb, - ,nan, "", isnan, -);

  TESTIT3 (significand, ,inf, , isinf, );
  TESTIT3 (significand, - ,inf, , isinf, -);
  TESTIT3 (significand,  ,nan, "", isnan, );
  TESTIT3 (significand, - ,nan, "", isnan, -);
}

int main()
{
  foo ();
  
  return 0;
}
