/* Copyright (C) 2006  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  October 23, 2006.  */

/* { dg-do link } */

/* Define "e" with as many bits as found in builtins.c:dconste.  */
#define M_E  2.7182818284590452353602874713526624977572470936999595749669676277241

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* Test that FUNC(ARG) == (RES).  */
#define TESTIT(FUNC,ARG,RES) do { \
  if (__builtin_##FUNC##f(ARG##F) != RES##F \
      || CKSGN_F(__builtin_##FUNC##f(ARG##F),RES##F)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != RES \
      || CKSGN(__builtin_##FUNC(ARG),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG##L) != RES##L \
      || CKSGN_L(__builtin_##FUNC##l(ARG##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Range test, check that (LOW) < FUNC(ARG) < (HI).  */
#define TESTIT_R(FUNC,ARG,LOW,HI) do { \
  if (__builtin_##FUNC##f(ARG) <= (LOW) || __builtin_##FUNC##f(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) <= (LOW) || __builtin_##FUNC(ARG) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) <= (LOW) || __builtin_##FUNC##l(ARG) >= (HI)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(ARG1, ARG2) == (RES).  */
#define TESTIT2(FUNC,ARG1,ARG2,RES) do { \
  if (__builtin_##FUNC##f(ARG1##F, ARG2##F) != RES##F \
      || CKSGN_F(__builtin_##FUNC##f(ARG1##F,ARG2##F),RES##F)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1, ARG2) != RES \
      || CKSGN(__builtin_##FUNC(ARG1,ARG2),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1##L, ARG2##L) != RES##L \
      || CKSGN_L(__builtin_##FUNC##l(ARG1##L,ARG2##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Range test, check that (LOW) < FUNC(ARG1,ARG2) < (HI).  */
#define TESTIT2_R(FUNC,ARG1,ARG2,LOW,HI) do { \
  if (__builtin_##FUNC##f(ARG1, ARG2) <= (LOW) \
      || __builtin_##FUNC##f(ARG1, ARG2) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1, ARG2) <= (LOW) \
      || __builtin_##FUNC(ARG1, ARG2) >= (HI)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1, ARG2) <= (LOW) \
      || __builtin_##FUNC##l(ARG1, ARG2) >= (HI)) \
    link_error(__LINE__); \
  } while (0)

/* Test that FUNC(ARG1, ARG2, ARG3) == (RES).  */
#define TESTIT3(FUNC,ARG1,ARG2,ARG3,RES) do { \
  if (__builtin_##FUNC##f(ARG1##F, ARG2##F, ARG3##F) != RES##F \
      || CKSGN_F(__builtin_##FUNC##f(ARG1##F,ARG2##F,ARG3##F),RES##F)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC(ARG1, ARG2, ARG3) != RES \
      || CKSGN(__builtin_##FUNC(ARG1,ARG2,ARG3),RES)) \
    link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG1##L, ARG2##L, ARG3##L) != RES##L \
      || CKSGN_L(__builtin_##FUNC##l(ARG1##L,ARG2##L,ARG3##L),RES##L)) \
    link_error(__LINE__); \
  } while (0)

/* Test that for FUNC(ARG, &ARG_S, &ARG_C);
   assert (ARG_S == RES_S && ARG_C == RES_C);.  */
#define TESTIT_2P(FUNC,ARG,ARG_S,ARG_C,RES_S,RES_C) do { \
  __builtin_##FUNC##f(ARG##F, &ARG_S##f, &ARG_C##f); \
  if (ARG_S##f != (RES_S##F) || ARG_C##f != (RES_C##F)) \
    link_error(__LINE__); \
  __builtin_##FUNC(ARG, &ARG_S, &ARG_C); \
  if (ARG_S != (RES_S) || ARG_C != (RES_C)) \
    link_error(__LINE__); \
  __builtin_##FUNC##l(ARG##L, &ARG_S##l, &ARG_C##l); \
  if (ARG_S##l != (RES_S##L) || ARG_C##l != (RES_C##L)) \
    link_error(__LINE__); \
  } while (0)

/* Test that for FUNC(ARG, &ARG_S, &ARG_C);
   assert (LOW_S < ARG_S < HI_S && LOW_C < ARG_C < HI_C);.  */
#define TESTIT_2P_R(FUNC,ARG,ARG_S,ARG_C,LOW_S,HI_S,LOW_C,HI_C) do { \
  __builtin_##FUNC##f(ARG##F, &ARG_S##f, &ARG_C##f); \
  if (ARG_S##f <= (LOW_S##F) || ARG_S##f >= (HI_S##F) \
      || ARG_C##f <= (LOW_C##F) || ARG_C##f >= (HI_C##F)) \
    link_error(__LINE__); \
  __builtin_##FUNC(ARG, &ARG_S, &ARG_C); \
  if (ARG_S <= (LOW_S) || ARG_S >= (HI_S) \
      || ARG_C <= (LOW_C) || ARG_C >= (HI_C)) \
    link_error(__LINE__); \
  __builtin_##FUNC##l(ARG##L, &ARG_S##l, &ARG_C##l); \
  if (ARG_S##l <= (LOW_S##L) || ARG_S##l >= (HI_S##L) \
      || ARG_C##l <= (LOW_C##L) || ARG_C##l >= (HI_C##L)) \
    link_error(__LINE__); \
  } while (0)

int main (void)
{
#ifdef __OPTIMIZE__
  float sf, cf, oneF = 1.0F;
  double s, c, one = 1.0;
  long double sl, cl, oneL = 1.0L;
#endif

  TESTIT_R (asin, -1.0, -3.15/2.0, -3.14/2.0); /* asin(-1) == -pi/2 */
  TESTIT (asin, 0.0, 0.0); /* asin(0) == 0 */
  TESTIT (asin, -0.0, -0.0); /* asin(-0) == -0 */
  TESTIT_R (asin, 1.0, 3.14/2.0, 3.15/2.0); /* asin(1) == pi/2 */

  TESTIT_R (acos, -1.0, 3.14, 3.15); /* acos(-1) == pi */
  TESTIT_R (acos, 0.0, 3.14/2.0, 3.15/2.0); /* acos(0) == pi/2 */
  TESTIT_R (acos, -0.0, 3.14/2.0, 3.15/2.0); /* acos(-0) == pi/2 */
  TESTIT (acos, 1.0, 0.0); /* acos(1) == 0 */

  TESTIT_R (atan, -1.0, -3.15/4.0, -3.14/4.0); /* atan(-1) == -pi/4 */
  TESTIT (atan, 0.0, 0.0); /* atan(0) == 0 */
  TESTIT (atan, -0.0, -0.0); /* atan(-0) == -0 */
  TESTIT_R (atan, 1.0, 3.14/4.0, 3.15/4.0); /* atan(1) == pi/4 */

  TESTIT_R (asinh, -1.0, -0.89, -0.88); /* asinh(-1) == -0.881... */
  TESTIT (asinh, 0.0, 0.0); /* asinh(0) == 0 */
  TESTIT (asinh, -0.0, -0.0); /* asinh(-0) == -0 */
  TESTIT_R (asinh, 1.0, 0.88, 0.89); /* asinh(1) == 0.881... */

  TESTIT (acosh, 1.0, 0.0); /* acosh(1) == 0. */
  TESTIT_R (acosh, 2.0, 1.31, 1.32); /* acosh(2) == 1.316... */

  TESTIT_R (atanh, -0.5, -0.55, -0.54); /* atanh(-0.5) == -0.549... */
  TESTIT (atanh, 0.0, 0.0); /* atanh(0) == 0 */
  TESTIT (atanh, -0.0, -0.0); /* atanh(-0) == -0 */
  TESTIT_R (atanh, 0.5, 0.54, 0.55); /* atanh(0.5) == 0.549... */

  TESTIT_R (sin, -1.0, -0.85, -0.84); /* sin(-1) == -0.841... */
  TESTIT (sin, 0.0, 0.0); /* sin(0) == 0 */
  TESTIT (sin, -0.0, -0.0); /* sin(-0) == -0 */
  TESTIT_R (sin, 1.0, 0.84, 0.85); /* sin(1) == 0.841... */

  TESTIT_R (cos, -1.0, 0.54, 0.55); /* cos(-1) == 0.5403... */
  TESTIT (cos, 0.0, 1.0); /* cos(0) == 1 */
  TESTIT (cos, -0.0, 1.0); /* cos(-0) == 1 */
  TESTIT_R (cos, 1.0, 0.54, 0.55); /* cos(1) == 0.5403... */

  TESTIT_R (tan, -1.0, -1.56, 1.55); /* tan(-1) == -1.557... */
  TESTIT (tan, 0.0, 0.0); /* tan(0) == 0 */
  TESTIT (tan, -0.0, -0.0); /* tan(-0) == -0 */
  TESTIT_R (tan, 1.0, 1.55, 1.56); /* tan(1) == 1.557... */

#ifdef __OPTIMIZE__
  /* These tests rely on propagating the variables s, c and one, which
     happens only when optimization is turned on.  */
  TESTIT_2P_R (sincos, -1.0, s, c, -0.85, -0.84, 0.54, 0.55); /* (s==-0.841..., c==0.5403...) */
  TESTIT_2P (sincos, 0.0, s, c, 0.0, 1.0); /* (s==0, c==1) */
  TESTIT_2P (sincos, -0.0, s, c, -0.0, 1.0); /* (s==-0, c==1) */
  TESTIT_2P_R (sincos, 1.0, s, c, 0.84, 0.85, 0.54, 0.55); /* (s==0.841..., c==0.5403...) */
  TESTIT_2P_R (sincos, one, s, c, 0.84, 0.85, 0.54, 0.55); /* (s==0.841..., c==0.5403...) */
  TESTIT_2P_R (sincos, -one, s, c, -0.85, -0.84, 0.54, 0.55); /* (s==-0.841..., c==0.5403...) */
#endif
  
  TESTIT_R (sinh, -1.0, -1.18, -1.17); /* sinh(-1) == -1.175... */
  TESTIT (sinh, 0.0, 0.0); /* sinh(0) == 0 */
  TESTIT (sinh, -0.0, -0.0); /* sinh(-0) == -0 */
  TESTIT_R (sinh, 1.0, 1.17, 1.18); /* sinh(1) == 1.175... */

  TESTIT_R (cosh, -1.0, 1.54, 1.55); /* cosh(-1) == 1.543... */
  TESTIT (cosh, 0.0, 1.0); /* cosh(0) == 1 */
  TESTIT (cosh, -0.0, 1.0); /* cosh(-0) == 1 */
  TESTIT_R (cosh, 1.0, 1.54, 1.55); /* cosh(1) == 1.543... */

  TESTIT_R (tanh, -1.0, -0.77, -0.76); /* tanh(-1) == -0.761... */
  TESTIT (tanh, -0.0, -0.0); /* tanh(-0) == -0 */
  TESTIT (tanh, 0.0, 0.0); /* tanh(0) == 0 */
  TESTIT_R (tanh, 1.0, 0.76, 0.77); /* tanh(1) == 0.761... */

  TESTIT_R (exp, -1.0, 0.36, 0.37); /* exp(-1) == 1/e */
  TESTIT (exp, -0.0, 1.0); /* exp(-0) == 1 */
  TESTIT (exp, 0.0, 1.0); /* exp(0) == 1 */
  TESTIT_R (exp, 1.0, 2.71, 2.72); /* exp(1) == e */

  TESTIT (exp2, -1.0, 0.5); /* exp2(-1) == 1/2 */
  TESTIT (exp2, -0.0, 1.0); /* exp2(-0) == 1 */
  TESTIT (exp2, 0.0, 1.0); /* exp2(0) == 1 */
  TESTIT (exp2, 1.0, 2.0); /* exp2(1) == 2 */

  TESTIT (exp10, -1.0, 0.1); /* exp10(-1) == 1/10 */
  TESTIT (exp10, -0.0, 1.0); /* exp10(-0) == 1 */
  TESTIT (exp10, 0.0, 1.0); /* exp10(0) == 1 */
  TESTIT (exp10, 1.0, 10.0); /* exp10(1) == 10 */

  TESTIT (pow10, -1.0, 0.1); /* pow10(-1) == 1/10 */
  TESTIT (pow10, -0.0, 1.0); /* pow10(-0) == 1 */
  TESTIT (pow10, 0.0, 1.0); /* pow10(0) == 1 */
  TESTIT (pow10, 1.0, 10.0); /* pow10(1) == 10 */

  TESTIT_R (expm1, -1.0, -0.64, -0.63); /* expm1(-1) == 1/e - 1 */
  TESTIT (expm1, -0.0, -0.0); /* expm1(-0) == 0 */
  TESTIT (expm1, 0.0, 0.0); /* expm1(0) == 0 */
  TESTIT_R (expm1, 1.0, 1.71, 1.72); /* expm1(1) == e - 1 */

  TESTIT (log, 1.0, 0.0); /* log(1) == 0 */
  TESTIT_R (log, M_E, 0.99, 1.01); /* log(e) == 1.000... */
  TESTIT_R (log, M_E*M_E, 1.99, 2.01); /* log(e*e) == 2.000... */

  TESTIT (log2, 1.0, 0.0); /* log2(1) == 0 */
  TESTIT (log2, 2.0, 1.0); /* log2(2) == 1 */
  TESTIT (log2, 4.0, 2.0); /* log2(4) == 2 */

  TESTIT (log10, 1.0, 0.0); /* log10(1) == 0 */
  TESTIT (log10, 10.0, 1.0); /* log10(10) == 1 */
  TESTIT (log10, 100.0, 2.0); /* log10(100) == 2 */

  TESTIT (log1p, 0.0, 0.0); /* log1p(0) == 0 */
  TESTIT (log1p, -0.0, -0.0); /* log1p(-0) == -0 */
  TESTIT_R (log1p, M_E-1, 0.99, 1.01); /* log1p(e-1) == 1.000... */
  TESTIT_R (log1p, M_E*M_E-1, 1.99, 2.01); /* log1p(e*e-1) == 2.000... */

  TESTIT (cbrt, -0.0, -0.0); /* cbrt(-0) == -0 */
  TESTIT (cbrt, 0.0, 0.0); /* cbrt(0) == 0 */
  TESTIT (cbrt, 1.0, 1.0); /* cbrt(1) == 1 */
  TESTIT (cbrt, -1.0, -1.0); /* cbrt(-1) == -1 */
  TESTIT (cbrt, 8.0, 2.0); /* cbrt(8) == 2 */
  TESTIT (cbrt, -8.0, -2.0); /* cbrt(-8) == -2 */

  TESTIT (erf, -0.0, -0.0); /* erf(-0) == -0 */
  TESTIT (erf, 0.0, 0.0); /* erf(0) == 0 */
  TESTIT_R (erf, 1.0, 0.84, 0.85); /* erf(1) == 0.842... */
  TESTIT_R (erf, -1.0, -0.85, -0.84); /* erf(-1) == -0.842... */

  TESTIT (erfc, -0.0, 1.0); /* erfc(-0) == 1 */
  TESTIT (erfc, 0.0, 1.0); /* erfc(0) == 1 */
  TESTIT_R (erfc, 1.0, 0.15, 0.16); /* erfc(1) == 0.157... */
  TESTIT_R (erfc, -1.0, 1.84, 1.85); /* erfc(-1) == 1.842... */

  TESTIT_R (tgamma, -4.5, -0.061, -0.060); /* tgamma(-4.5) == -0.06001... */
  TESTIT_R (tgamma, -3.5, 0.27, 0.28); /* tgamma(-3.5) == 0.27008... */
  TESTIT_R (tgamma, -2.5, -0.95, -0.94); /* tgamma(-2.5) == -0.945... */
  TESTIT_R (tgamma, -1.5, 2.36, 2.37); /* tgamma(-1.5) == 2.363... */
  TESTIT_R (tgamma, -0.5, -3.55, -3.54); /* tgamma(-0.5) == -3.544... */
  TESTIT_R (tgamma, 0.5, 1.77, 1.78); /* tgamma(0.5) == 1.772... */
  TESTIT (tgamma, 1.0, 1.0); /* tgamma(1) == 1 */
  TESTIT_R (tgamma, 1.5, 0.88, 0.89); /* tgamma(1.5) == 0.886... */
  TESTIT (tgamma, 2.0, 1.0); /* tgamma(2) == 1 */
  TESTIT_R (tgamma, 2.5, 1.32, 1.33); /* tgamma(2.5) == 1.329... */
  TESTIT (tgamma, 3.0, 2.0); /* tgamma(3) == 2 */
  TESTIT_R (tgamma, 3.5, 3.32, 3.33); /* tgamma(3.5) == 3.323... */
  TESTIT (tgamma, 4.0, 6.0); /* tgamma(4) == 6 */
  TESTIT_R (tgamma, 4.5, 11.63, 11.64); /* tgamma(4.5) == 11.631... */

  TESTIT2 (pow, 3.0, 4.0, 81.0); /* pow(3,4) == 81 */
  TESTIT2 (pow, -3.0, 5.0, -243.0); /* pow(-3,5) == -243 */
  TESTIT2 (pow, 16.0, 0.25, 2.0); /* pow(16,1/4) == 2 */
  TESTIT2 (pow, 4.0, -2.0, 0.0625); /* pow(4,-2) == 1/16 */
  TESTIT2 (pow, -2.0, -3.0, -0.125); /* pow(-2,-3) == -1/8 */
  TESTIT2_R (pow, -1.5, -3.0, -0.297, -0.296); /* pow(-1.5,-3) == -1/3.375 */

  TESTIT2 (hypot, 0.0, 0.0, 0.0); /* hypot(0,0) == 0 */
  TESTIT2 (hypot, -0.0, 0.0, 0.0); /* hypot(-0,0) == 0 */
  TESTIT2 (hypot, 0.0, -0.0, 0.0); /* hypot(0,-0) == 0 */
  TESTIT2 (hypot, -0.0, -0.0, 0.0); /* hypot(-0,-0) == 0 */
  TESTIT2 (hypot, 3.0, 4.0, 5.0); /* hypot(3,4) == 5 */
  TESTIT2 (hypot, -3.0, 4.0, 5.0); /* hypot(-3,4) == 5 */
  TESTIT2 (hypot, 3.0, -4.0, 5.0); /* hypot(3,-4) == 5 */
  TESTIT2 (hypot, -3.0, -4.0, 5.0); /* hypot(-3,-4) == 5 */
  TESTIT2_R (hypot, 4.0, 5.0, 6.40, 6.41); /* hypot(4,5) == 6.403... */

  TESTIT2 (atan2, 0.0, 0.0, 0.0); /* atan2(0,0) == 0 */
  TESTIT2 (atan2, -0.0, 0.0, -0.0); /* atan2(-0,0) == -0 */
  TESTIT2_R (atan2, 0.0, -0.0, 3.14, 3.15); /* atan2(0,-0) == pi */
  TESTIT2_R (atan2, -0.0, -0.0, -3.15, -3.14); /* atan2(-0,-0) == -pi */
  TESTIT2_R (atan2, 0.0, -1.0, 3.14, 3.15); /* atan2(0,-1) == pi */
  TESTIT2_R (atan2, -0.0, -1.0, -3.15, -3.14); /* atan2(-0,-1) == -pi */
  TESTIT2 (atan2, 0.0, 1.0, 0.0); /* atan2(0,1) == 0 */
  TESTIT2 (atan2, -0.0, 1.0, -0.0); /* atan2(-0,1) == -0 */
  TESTIT2_R (atan2, -1.0, 0.0, -1.58, -1.57); /* atan2(-1,0) == -pi/2 */
  TESTIT2_R (atan2, 1.0, 0.0, 1.57, 1.58); /* atan2(1,0) == pi/2 */

  TESTIT2 (fdim, 0.0, 0.0, 0.0); /* fdim(0,0) == 0 */
  TESTIT2 (fdim, -0.0, 0.0, 0.0); /* fdim(-0,0) == 0 */
  TESTIT2 (fdim, 0.0, -0.0, 0.0); /* fdim(0,-0) == 0 */
  TESTIT2 (fdim, -0.0, -0.0, 0.0); /* fdim(-0,-0) == 0 */
  TESTIT2 (fdim, 5.0, 5.0, 0.0); /* fdim(5,5) == 0 */
  TESTIT2 (fdim, 5.0, 6.0, 0.0); /* fdim(5,6) == 0 */
  TESTIT2 (fdim, 6.0, 5.0, 1.0); /* fdim(6,5) == 1 */
  TESTIT2 (fdim, -5.0, -6.0, 1.0); /* fdim(-5,-6) == 1 */
  TESTIT2 (fdim, -6.0, -5.0, 0.0); /* fdim(-6,-5) == 0 */

  TESTIT2 (fmin, 5.0, 6.0, 5.0); /* fmin(5,6) == 5 */
  TESTIT2 (fmin, 6.0, 5.0, 5.0); /* fmin(6,5) == 5 */
  TESTIT2 (fmin, -5.0, -6.0, -6.0); /* fmin(-5,-6) == -6 */
  TESTIT2 (fmin, -6.0, -5.0, -6.0); /* fmin(-6,-5) == -6 */
  TESTIT2 (fmin, -0.0, 0.0, -0.0); /* fmin(-0,0) == -0 */
  TESTIT2 (fmin, 0.0, -0.0, -0.0); /* fmin(-0,0) == -0 */

  TESTIT2 (fmax, 5.0, 6.0, 6.0); /* fmax(5,6) == 6 */
  TESTIT2 (fmax, 6.0, 5.0, 6.0); /* fmax(6,5) == 6 */
  TESTIT2 (fmax, -5.0, -6.0, -5.0); /* fmax(-5,-6) == -5 */
  TESTIT2 (fmax, -6.0, -5.0, -5.0); /* fmax(-6,-5) == -5 */
  TESTIT2 (fmax, -0.0, 0.0, 0.0); /* fmax(-0,0) == 0 */
  TESTIT2 (fmax, 0.0, -0.0, 0.0); /* fmax(-0,0) == 0 */

  TESTIT3 (fma, 2.0, 3.0, 4.0, 10.0); /* fma(2,3,4) == 10 */
  TESTIT3 (fma, 2.0, -3.0, 4.0, -2.0); /* fma(2,-3,4) == -2 */
  TESTIT3 (fma, 2.0, 3.0, -4.0, 2.0); /* fma(2,3,-4) == 2 */
  TESTIT3 (fma, 2.0, -3.0, -4.0, -10.0); /* fma(2,-3,-4) == -10 */
  TESTIT3 (fma, -2.0, -3.0, -4.0, 2.0); /* fma(-2,-3,-4) == 2 */
  TESTIT3 (fma, 6.0, -0.0, 0.0, 0.0); /* fma(6,-0,0) == 0 */
  TESTIT3 (fma, -0.0, 6.0, 0.0, 0.0); /* fma(-0,6,0) == 0 */
  TESTIT3 (fma, 6.0, -0.0, -0.0, -0.0); /* fma(6,-0,-0) == -0 */
  TESTIT3 (fma, -0.0, 6.0, -0.0, -0.0); /* fma(-0,6,-0) == -0 */
  TESTIT3 (fma, 0.0, 0.0, 0.0, 0.0); /* fma(0,0,0) == 0 */
  TESTIT3 (fma, -0.0, 0.0, 0.0, 0.0); /* fma(-0,0,0) == 0 */
  TESTIT3 (fma, 0.0, -0.0, 0.0, 0.0); /* fma(0,-0,0) == 0 */
  TESTIT3 (fma, -0.0, -0.0, 0.0, 0.0); /* fma(-0,-0,0) == 0 */
  TESTIT3 (fma, 0.0, 0.0, -0.0, 0.0); /* fma(0,0,-0) == 0 */
  TESTIT3 (fma, -0.0, 0.0, -0.0, -0.0); /* fma(-0,0,-0) == -0 */
  TESTIT3 (fma, 0.0, -0.0, -0.0, -0.0); /* fma(0,-0,-0) == -0 */
  TESTIT3 (fma, -0.0, -0.0, -0.0, 0.0); /* fma(-0,-0,-0) == 0 */

  if (__builtin_fmaf(__FLT_MAX__, 2.0F, -__FLT_MAX__) != __FLT_MAX__)
    link_error (__LINE__);
  if (__builtin_fmaf(2.0F,__FLT_MAX__, -__FLT_MAX__) != __FLT_MAX__)
    link_error (__LINE__);
  if (__builtin_fmaf(__FLT_MIN__, 0.5F, __FLT_MIN__) != __FLT_MIN__*1.5F)
    link_error (__LINE__);
  if (__builtin_fmaf(0.5F,__FLT_MIN__, __FLT_MIN__) != __FLT_MIN__*1.5F)
    link_error (__LINE__);

  if (__builtin_fma(__DBL_MAX__, 2.0, -__DBL_MAX__) != __DBL_MAX__)
    link_error (__LINE__);
  if (__builtin_fma(2.0,__DBL_MAX__, -__DBL_MAX__) != __DBL_MAX__)
    link_error (__LINE__);
  if (__builtin_fma(__DBL_MIN__, 0.5, __DBL_MIN__) != __DBL_MIN__*1.5)
    link_error (__LINE__);
  if (__builtin_fma(0.5,__DBL_MIN__, __DBL_MIN__) != __DBL_MIN__*1.5)
    link_error (__LINE__);

  if (__builtin_fmal(__LDBL_MAX__, 2.0L, -__LDBL_MAX__) != __LDBL_MAX__)
    link_error (__LINE__);
  if (__builtin_fmal(2.0L,__LDBL_MAX__, -__LDBL_MAX__) != __LDBL_MAX__)
    link_error (__LINE__);
  if (__builtin_fmal(__LDBL_MIN__, 0.5L, __LDBL_MIN__) != __LDBL_MIN__*1.5L)
    link_error (__LINE__);
  if (__builtin_fmal(0.5L,__LDBL_MIN__, __LDBL_MIN__) != __LDBL_MIN__*1.5L)
    link_error (__LINE__);

  TESTIT (sqrt, -0.0, -0.0); /* sqrt(-0) == -0 */
  TESTIT (sqrt, 0.0, 0.0); /* sqrt(0) == 0 */
  TESTIT (sqrt, 1.0, 1.0); /* sqrt(1) == 1 */
  TESTIT (sqrt, 4.0, 2.0); /* sqrt(4) == 2 */
  TESTIT_R (sqrt, 1.5, 1.22, 1.23); /* sqrt(1.5) == 1.224... */
  TESTIT_R (sqrt, 2.0, 1.41, 1.42); /* sqrt(2) == 1.414... */

  return 0;
}
