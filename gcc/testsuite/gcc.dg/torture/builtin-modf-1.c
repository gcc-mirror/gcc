/* Copyright (C) 2007  Free Software Foundation.

   Verify that built-in folding of modf is correctly performed by the
   compiler.

   Origin: Kaveh R. Ghazi,  February 23, 2007.  */

/* { dg-do link } */
/* { dg-options "-fno-finite-math-only" { target sh*-*-* } } */
/* { dg-options "-funsafe-math-optimizations -fsigned-zeros -fno-associative-math" { target powerpc-*-darwin* powerpc*-*-linux* } } */

extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* We use these macros if we can only check these when optimizing.  In
   some cases we rely on other optimizations to propagate the value
   and fold away certain constructs.  Likewise for the sign testing.
   TRUE means an error occurred.  */
#ifdef __OPTIMIZE__
#define CKRES(X) (X)
#define CKIPTR(X,Y) X != Y
#define CKSGN_IPTR_F(X,Y) CKSGN_F(X,Y)
#define CKSGN_IPTR(X,Y) CKSGN(X,Y)
#define CKSGN_IPTR_L(X,Y) CKSGN_L(X,Y)
#else
#define CKRES(X) 0
#define CKIPTR(X,Y) 0
#define CKSGN_IPTR_F(X,Y) 0
#define CKSGN_IPTR(X,Y) 0
#define CKSGN_IPTR_L(X,Y) 0
#endif

/* Test that modf(ARG1,&iptr) == FRACRES && iptr == INTRES.  Check the
   sign in case we get -0.0.  */
#define TESTIT_MODF(ARG,INTRES,FRACRES) do { \
  float iptrf = 0.5; double iptr = 0.5; long double iptrl = 0.5; \
  if (__builtin_modff(ARG##f,&iptrf) != FRACRES##f \
      || CKIPTR(iptrf,INTRES##f) \
      || CKSGN_F(__builtin_modff(ARG##f,&iptrf),FRACRES##f) \
      || CKSGN_IPTR_F(iptrf,INTRES##f)) \
    link_error(__LINE__); \
  if (__builtin_modf(ARG,&iptr) != FRACRES \
      || CKIPTR(iptr,INTRES) \
      || CKSGN(__builtin_modf(ARG,&iptr),FRACRES) \
      || CKSGN_IPTR(iptr,INTRES)) \
    link_error(__LINE__); \
  if (__builtin_modfl(ARG##l,&iptrl) != FRACRES##l \
      || CKIPTR(iptrl,INTRES##l) \
      || CKSGN_L(__builtin_modfl(ARG##l,&iptrl),FRACRES##l) \
      || CKSGN_IPTR_L(iptrl,INTRES##l)) \
    link_error(__LINE__); \
  } while (0)

/* Test that modf(NEG FUNCARG(ARGARG, &iptr)) == FRACRES &&
   FUNCRES(iptr) is true.  Check the sign of both as well.  This is
   for checking an argument of Inf.  */
#ifndef __SPU__
#define TESTIT_MODF2(NEG,FUNCARG,ARGARG,FUNCRES,FRACRES) do { \
  float iptrf = 0.5; double iptr = 0.5; long double iptrl = 0.5; \
  if (__builtin_modff(NEG __builtin_##FUNCARG##f(ARGARG),&iptrf) != FRACRES##f \
      || CKSGN_F(__builtin_modff(NEG __builtin_##FUNCARG##f(ARGARG),&iptrf), FRACRES##f) \
      || CKIPTR(!__builtin_##FUNCRES##f(iptrf),0) \
      || CKSGN_IPTR_F(iptrf,FRACRES##f)) \
    link_error(__LINE__); \
  if (__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr) != FRACRES \
      || CKSGN(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr), FRACRES) \
      || CKIPTR(!__builtin_##FUNCRES(iptr),0) \
      || CKSGN_IPTR(iptr,FRACRES)) \
    link_error(__LINE__); \
  if (__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl) != FRACRES##l \
      || CKSGN_L(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl), FRACRES##l) \
      || CKIPTR(!__builtin_##FUNCRES##l(iptrl),0) \
      || CKSGN_IPTR_L(iptrl,FRACRES##l)) \
    link_error(__LINE__); \
  } while (0)
#else
#define TESTIT_MODF2(NEG,FUNCARG,ARGARG,FUNCRES,FRACRES) do { \
  /* SPU single-precision floating point format does not support Inf or Nan.  */ \
  double iptr = 0.5; long double iptrl = 0.5; \
  if (__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr) != FRACRES \
      || CKSGN(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr), FRACRES) \
      || CKIPTR(!__builtin_##FUNCRES(iptr),0) \
      || CKSGN_IPTR(iptr,FRACRES)) \
    link_error(__LINE__); \
  if (__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl) != FRACRES##l \
      || CKSGN_L(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl), FRACRES##l) \
      || CKIPTR(!__builtin_##FUNCRES##l(iptrl),0) \
      || CKSGN_IPTR_L(iptrl,FRACRES##l)) \
    link_error(__LINE__); \
  } while (0)
#endif

/* Test that FUNCRES(modf(NEG FUNCARG(ARGARG, &iptr))) is true &&
   FUNCRES(iptr) is true.  Check the sign of both as well.  This is
   for checking an argument of NaN.  */
#ifndef __SPU__
#define TESTIT_MODF3(NEG,FUNCARG,ARGARG,FUNCRES) do { \
  float iptrf = 0.5; double iptr = 0.5; long double iptrl = 0.5; \
  if (CKRES(!__builtin_##FUNCRES##f(__builtin_modff(NEG __builtin_##FUNCARG##f(ARGARG),&iptrf))) \
      || CKSGN_F(__builtin_modff(NEG __builtin_##FUNCARG##f(ARGARG),&iptrf), NEG 1) \
      || CKIPTR(!__builtin_##FUNCRES##f(iptrf),0) \
      || CKSGN_IPTR_F(iptrf,NEG 1)) \
    link_error(__LINE__); \
  if (CKRES(!__builtin_##FUNCRES(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr))) \
      || CKSGN(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr), NEG 1) \
      || CKIPTR(!__builtin_##FUNCRES(iptr),0) \
      || CKSGN_IPTR(iptr,NEG 1)) \
    link_error(__LINE__); \
  if (CKRES(!__builtin_##FUNCRES##l(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl))) \
      || CKSGN_L(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl), NEG 1) \
      || CKIPTR(!__builtin_##FUNCRES##l(iptrl),0) \
      || CKSGN_IPTR_L(iptrl,NEG 1)) \
    link_error(__LINE__); \
  } while (0)
#else
#define TESTIT_MODF3(NEG,FUNCARG,ARGARG,FUNCRES) do { \
  /* SPU single-precision floating point format does not support Inf or Nan.  */ \
  double iptr = 0.5; long double iptrl = 0.5; \
  if (CKRES(!__builtin_##FUNCRES(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr))) \
      || CKSGN(__builtin_modf(NEG __builtin_##FUNCARG(ARGARG),&iptr), NEG 1) \
      || CKIPTR(!__builtin_##FUNCRES(iptr),0) \
      || CKSGN_IPTR(iptr,NEG 1)) \
    link_error(__LINE__); \
  if (CKRES(!__builtin_##FUNCRES##l(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl))) \
      || CKSGN_L(__builtin_modfl(NEG __builtin_##FUNCARG##l(ARGARG),&iptrl), NEG 1) \
      || CKIPTR(!__builtin_##FUNCRES##l(iptrl),0) \
      || CKSGN_IPTR_L(iptrl,NEG 1)) \
    link_error(__LINE__); \
  } while (0)
#endif

void __attribute__ ((__noinline__))
foo(void)
{
  /* Test that modf(ARG1,&iptr) -> ARG3 && iptr == ARG2.  */
  TESTIT_MODF (0x1p10F+0.5, 0x1p10, 0.5);
  TESTIT_MODF (0x1p10F+0x1p-10, 0x1p10, 0x1p-10);
  TESTIT_MODF (12345678L/17.0, 726216.0, -726216L+12345678L/17.0);
  TESTIT_MODF (555.555, 555.0, -555+555.555);
  TESTIT_MODF (5000/11.0, 454.0, -454+5000/11.0);
  TESTIT_MODF (1000/7.0, 142.0, -142+1000/7.0);
  TESTIT_MODF (123/7.0, 17.0, -17+123/7.0);
  TESTIT_MODF (117/7.0, 16.0, -16+117/7.0);
  TESTIT_MODF (5.5, 5.0, 0.5);
  TESTIT_MODF (1.5, 1.0, 0.5);
  TESTIT_MODF (4/3.0, 1.0, -1+4/3.0);
  TESTIT_MODF (1.0, 1.0, 0.0);
  TESTIT_MODF (0.5, 0.0, 0.5);
  TESTIT_MODF (4/9.0, 0.0, 4/9.0);
  TESTIT_MODF (1/3.0, 0.0, 1/3.0);
  TESTIT_MODF (1/9.0, 0.0, 1/9.0);
  TESTIT_MODF (0.0, 0.0, 0.0);

  TESTIT_MODF (-0.0, -0.0, -0.0);
  TESTIT_MODF (-1/9.0, -0.0, -1/9.0);
  TESTIT_MODF (-1/3.0, -0.0, -1/3.0);
  TESTIT_MODF (-4/9.0, -0.0, -4/9.0);
  TESTIT_MODF (-0.5, -0.0, -0.5);
  TESTIT_MODF (-1.0, -1.0, -0.0);
  TESTIT_MODF (-4/3.0, -1.0, 1-4/3.0);
  TESTIT_MODF (-1.5, -1.0, -0.5);
  TESTIT_MODF (-5.5, -5.0, -0.5);
  TESTIT_MODF (-117/7.0, -16.0, 16-117/7.0);
  TESTIT_MODF (-123/7.0, -17.0, 17-123/7.0);
  TESTIT_MODF (-1000/7.0, -142.0, 142-1000/7.0);
  TESTIT_MODF (-5000/11.0, -454.0, 454-5000/11.0);
  TESTIT_MODF (-555.555, -555.0, 555-555.555);
  TESTIT_MODF (-12345678L/17.0, -726216.0, 726216L-12345678L/17.0);
  TESTIT_MODF (-0x1p10F-0x1p-10, -0x1p10, -0x1p-10);
  TESTIT_MODF (-0x1p10F-0.5, -0x1p10, -0.5);

  
  /* Test for modf(+-Inf,&i) -> (i=+-0.0, +-Inf).  */
  TESTIT_MODF2 ( ,inf, , isinf, 0.0);
  TESTIT_MODF2 (- ,inf, , isinf, -0.0);

  /* Test for and modf(+-NaN,&i) -> (i=+-NaN, +-NaN).  */
  TESTIT_MODF3 ( ,nan, "", isnan);
  TESTIT_MODF3 (- ,nan, "", isnan);
}

int main()
{
  foo();
  
  return 0;
}
