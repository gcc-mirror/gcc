/* Copyright (C) 2006, 2007  Free Software Foundation.

   Test things that should block GCC from optimizing compile-time
   constants passed to a builtin transcendental function.

   Origin: Kaveh R. Ghazi,  October 22, 2006.  */

/* { dg-do compile } */
/* { dg-require-effective-target inf } */
/* { dg-options "-fdump-tree-original" } */

extern void foof (float);
extern void foo (double);
extern void fool (long double);

#define TESTIT(FUNC, ARG) do { \
  foof (__builtin_##FUNC##f (ARG##F)); \
  foo (__builtin_##FUNC (ARG)); \
  fool (__builtin_##FUNC##l (ARG##L)); \
} while (0)

#define TESTIT2(FUNC, ARG1, ARG2) do { \
  foof (__builtin_##FUNC##f (ARG1##F, ARG2##F)); \
  foo (__builtin_##FUNC (ARG1, ARG2)); \
  fool (__builtin_##FUNC##l (ARG1##L, ARG2##L)); \
} while (0)

#define TESTIT2_I1(FUNC, ARG1, ARG2) do { \
  foof (__builtin_##FUNC##f (ARG1, ARG2##F)); \
  foo (__builtin_##FUNC (ARG1, ARG2)); \
  fool (__builtin_##FUNC##l (ARG1, ARG2##L)); \
} while (0)

#define TESTIT2_I2ALL(FUNC, ARGF, MAXF, ARGD, MAXD, ARGLD, MAXLD) do { \
  foof (__builtin_##FUNC##f (ARGF, MAXF)); \
  foo (__builtin_##FUNC (ARGD, MAXD)); \
  fool (__builtin_##FUNC##l (ARGLD, MAXLD)); \
} while (0)

#define TESTIT2_I2(FUNC, ARG1, ARG2) do { \
  foof (__builtin_##FUNC##f (ARG1##F, ARG2)); \
  foo (__builtin_##FUNC (ARG1, ARG2)); \
  fool (__builtin_##FUNC##l (ARG1##L, ARG2)); \
} while (0)

#define TESTIT_REMQUO(ARG1, ARG2) do { \
  int quo; \
  foof (__builtin_remquof (ARG1##F, ARG2##F, &quo)); \
  foo (__builtin_remquo (ARG1, ARG2, &quo)); \
  fool (__builtin_remquol (ARG1##L, ARG2##L, &quo)); \
} while (0)

#define TESTIT_REENT(FUNC,ARG1) do { \
  int sg; \
  foof (__builtin_##FUNC##f_r (ARG1##F, &sg)); \
  foo (__builtin_##FUNC##_r (ARG1, &sg)); \
  fool (__builtin_##FUNC##l_r (ARG1##L, &sg)); \
} while (0)

void bar()
{
  /* An argument of NaN is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_exp2f (__builtin_nanf("")));
#endif
  foo (__builtin_exp2 (__builtin_nan("")));
  fool (__builtin_exp2l (__builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_exp2f (__builtin_inff()));
#endif
  foo (__builtin_exp2 (__builtin_inf()));
  fool (__builtin_exp2l (__builtin_infl()));
#ifndef __SPU__
  foof (__builtin_exp2f (-__builtin_inff()));
#endif
  foo (__builtin_exp2 (-__builtin_inf()));
  fool (__builtin_exp2l (-__builtin_infl()));

  /* Result overflows MPFR, which in version 2.2.x has 30 exponent bits.  */
  TESTIT (exp2, 0x1p50);
  /* Result underflows MPFR, which in version 2.2.x has 30 exponent bits.  */
  TESTIT (exp2, -0x1p50);

  /* Result overflows GCC's REAL_VALUE_TYPE, which has 26 exponent bits.  */
  TESTIT (exp2, 0x1p28);
  /* Result underflows GCC's REAL_VALUE_TYPE, which has 26 exponent bits.  */
  TESTIT (exp2, -0x1p28);
  
  /* Result overflows (even an extended) C double's mode.  */
  TESTIT (exp2, 0x1p24);
  /* Result underflows (even an extended) C double's mode.  */
  TESTIT (exp2, -0x1p24);

  /* Ensure that normal arguments/results are folded.  */
  TESTIT (exp2, 1.5);
  TESTIT (exp2, -1.5);
  
  /* The asin arg must be [-1 ... 1] inclusive.  */
  TESTIT (asin, -1.5);
  TESTIT (asin, 1.5);

  /* The acos arg must be [-1 ... 1] inclusive.  */
  TESTIT (acos, -1.5);
  TESTIT (acos, 1.5);
  
  /* The acosh arg must be [1 ... Inf] inclusive.  */
  TESTIT (acosh, 0.5);

  /* The atanh arg must be [-1 ... 1] EXclusive.  */
  TESTIT (atanh, -1.0);
  TESTIT (atanh, 1.0);

  /* The log* arg must be [0 ... Inf] EXclusive.  */
  TESTIT (log, -1.0);
  TESTIT (log, 0.0);
  TESTIT (log, -0.0);
  
  TESTIT (log2, -1.0);
  TESTIT (log2, 0.0);
  TESTIT (log2, -0.0);
  
  TESTIT (log10, -1.0);
  TESTIT (log10, 0.0);
  TESTIT (log10, -0.0);
  
  /* The log1p arg must be [-1 ... Inf] EXclusive.  */
  TESTIT (log1p, -2.0);
  TESTIT (log1p, -1.0);

  /* The tgamma arg errors with zero or negative integers.  */
  TESTIT (tgamma, 0.0);
  TESTIT (tgamma, -0.0);
  TESTIT (tgamma, -1.0);
  TESTIT (tgamma, -2.0);
  TESTIT (tgamma, -3.0);

  /* An argument of NaN is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_powf (__builtin_nanf(""), 2.5F));
#endif
  foo (__builtin_pow (__builtin_nan(""), 2.5));
  fool (__builtin_powl (__builtin_nanl(""), 2.5L));
#ifndef __SPU__
  foof (__builtin_powf (2.5F, __builtin_nanf("")));
#endif
  foo (__builtin_pow (2.5, __builtin_nan("")));
  fool (__builtin_powl (2.5L, __builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_powf (__builtin_inff(), 2.5F));
#endif
  foo (__builtin_pow (__builtin_inf(), 2.5));
  fool (__builtin_powl (__builtin_infl(), 2.5L));
#ifndef __SPU__
  foof (__builtin_powf (-__builtin_inff(), 2.5F));
#endif
  foo (__builtin_pow (-__builtin_inf(), 2.5));
  fool (__builtin_powl (-__builtin_infl(), 2.5L));
#ifndef __SPU__
  foof (__builtin_powf (2.5F, __builtin_inff()));
#endif
  foo (__builtin_pow (2.5, __builtin_inf()));
  fool (__builtin_powl (2.5L, __builtin_infl()));
#ifndef __SPU__
  foof (__builtin_powf (2.5F, -__builtin_inff()));
#endif
  foo (__builtin_pow (2.5, -__builtin_inf()));
  fool (__builtin_powl (2.5L, -__builtin_infl()));

  /* Check for Inv/NaN return values.  */
  TESTIT2 (pow, -0.0, -4.5); /* Returns Inf */
  TESTIT2 (pow, 0.0, -4.5); /* Returns Inf */
  TESTIT2 (pow, -3.0, -4.5); /* Returns NaN */

  /* Check for overflow/underflow.  */
  foof (__builtin_powf (__FLT_MAX__, 3.5F));
  foo (__builtin_pow (__DBL_MAX__, 3.5));
  fool (__builtin_powl (__LDBL_MAX__, 3.5L));
  TESTIT2 (pow, 2.0, 0x1p50);
  foof (__builtin_powf (__FLT_MAX__, -3.5F));
  foo (__builtin_pow (__DBL_MAX__, -3.5));
  fool (__builtin_powl (__LDBL_MAX__, -3.5L));
  TESTIT2 (pow, 2.0, -0x1p50);
  
  /* The sqrt arg must be [0 ... Inf] inclusive.  */
  TESTIT (sqrt, -0.5);
  TESTIT (sqrt, -0.0);
  TESTIT (sqrt, 0.0);

  /* Check for overflow/underflow.  */

  /* These adjustments are too big.  */
#define FLT_EXP_ADJ (2*(__FLT_MAX_EXP__-__FLT_MIN_EXP__)+1)
#define DBL_EXP_ADJ (2*(__DBL_MAX_EXP__-__DBL_MIN_EXP__)+1)
#define LDBL_EXP_ADJ (2*(__LDBL_MAX_EXP__-__LDBL_MIN_EXP__)+1)

  TESTIT2_I2 (ldexp, 1.0, __INT_MAX__);
  TESTIT2_I2 (ldexp, 1.0, -__INT_MAX__-1);
  TESTIT2_I2 (ldexp, -1.0, __INT_MAX__);
  TESTIT2_I2 (ldexp, -1.0, -__INT_MAX__-1);
  TESTIT2_I2ALL (ldexp, __FLT_MIN__, FLT_EXP_ADJ, __DBL_MIN__,
		 DBL_EXP_ADJ, __LDBL_MIN__, LDBL_EXP_ADJ);
  TESTIT2_I2ALL (ldexp, __FLT_MAX__, -FLT_EXP_ADJ, __DBL_MAX__,
		 -DBL_EXP_ADJ, __LDBL_MAX__, -LDBL_EXP_ADJ);
  TESTIT2_I2ALL (ldexp, __FLT_MIN__, __FLT_MIN_EXP__, __DBL_MIN__,
		 __DBL_MIN_EXP__, __LDBL_MIN__, __LDBL_MIN_EXP__);
  TESTIT2_I2ALL (ldexp, __FLT_MAX__, __FLT_MAX_EXP__, __DBL_MAX__,
		 __DBL_MAX_EXP__, __LDBL_MAX__, __LDBL_MAX_EXP__);

  TESTIT2_I2 (scalbn, 1.0, __INT_MAX__);
  TESTIT2_I2 (scalbn, 1.0, -__INT_MAX__-1);
  TESTIT2_I2 (scalbn, -1.0, __INT_MAX__);
  TESTIT2_I2 (scalbn, -1.0, -__INT_MAX__-1);
  TESTIT2_I2ALL (scalbn, __FLT_MIN__, FLT_EXP_ADJ, __DBL_MIN__,
		 DBL_EXP_ADJ, __LDBL_MIN__, LDBL_EXP_ADJ);
  TESTIT2_I2ALL (scalbn, __FLT_MAX__, -FLT_EXP_ADJ, __DBL_MAX__,
		 -DBL_EXP_ADJ, __LDBL_MAX__, -LDBL_EXP_ADJ);
  TESTIT2_I2ALL (scalbn, __FLT_MIN__, __FLT_MIN_EXP__, __DBL_MIN__,
		 __DBL_MIN_EXP__, __LDBL_MIN__, __LDBL_MIN_EXP__);
  TESTIT2_I2ALL (scalbn, __FLT_MAX__, __FLT_MAX_EXP__, __DBL_MAX__,
		 __DBL_MAX_EXP__, __LDBL_MAX__, __LDBL_MAX_EXP__);

  TESTIT2_I2 (scalbln, 1.0, __LONG_MAX__);
  TESTIT2_I2 (scalbln, 1.0, -__LONG_MAX__-1);
  TESTIT2_I2 (scalbln, -1.0, __LONG_MAX__);
  TESTIT2_I2 (scalbln, -1.0, -__LONG_MAX__-1);
  TESTIT2_I2ALL (scalbln, __FLT_MIN__, FLT_EXP_ADJ, __DBL_MIN__,
		 DBL_EXP_ADJ, __LDBL_MIN__, LDBL_EXP_ADJ);
  TESTIT2_I2ALL (scalbln, __FLT_MAX__, -FLT_EXP_ADJ, __DBL_MAX__,
		 -DBL_EXP_ADJ, __LDBL_MAX__, -LDBL_EXP_ADJ);
  TESTIT2_I2ALL (scalbln, __FLT_MIN__, __FLT_MIN_EXP__, __DBL_MIN__,
		 __DBL_MIN_EXP__, __LDBL_MIN__, __LDBL_MIN_EXP__);
  TESTIT2_I2ALL (scalbln, __FLT_MAX__, __FLT_MAX_EXP__, __DBL_MAX__,
		 __DBL_MAX_EXP__, __LDBL_MAX__, __LDBL_MAX_EXP__);

  TESTIT (logb, 0.0);
  TESTIT (logb, -0.0);

  TESTIT (ilogb, 0.0);
  TESTIT (ilogb, -0.0);

#ifndef __SPU__
  foof (__builtin_ilogbf (__builtin_inff()));
#endif
  foo (__builtin_ilogb (__builtin_inf()));
  fool (__builtin_ilogbl (__builtin_infl()));
#ifndef __SPU__
  foof (__builtin_ilogbf (-__builtin_inff()));
#endif
  foo (__builtin_ilogb (-__builtin_inf()));
  fool (__builtin_ilogbl (-__builtin_infl()));

#ifndef __SPU__
  foof (__builtin_ilogbf (__builtin_nanf("")));
#endif
  foo (__builtin_ilogb (__builtin_nan("")));
  fool (__builtin_ilogbl (__builtin_nanl("")));
#ifndef __SPU__
  foof (__builtin_ilogbf (-__builtin_nanf("")));
#endif
  foo (__builtin_ilogb (-__builtin_nan("")));
  fool (__builtin_ilogbl (-__builtin_nanl("")));

  /* The y* arg must be [0 ... Inf] EXclusive.  */
  TESTIT (y0, -1.0);
  TESTIT (y0, 0.0);
  TESTIT (y0, -0.0);

  TESTIT (y1, -1.0);
  TESTIT (y1, 0.0);
  TESTIT (y1, -0.0);

  TESTIT2_I1 (yn, 2, -1.0);
  TESTIT2_I1 (yn, 2, 0.0);
  TESTIT2_I1 (yn, 2, -0.0);

  TESTIT2_I1 (yn, -3, -1.0);
  TESTIT2_I1 (yn, -3, 0.0);
  TESTIT2_I1 (yn, -3, -0.0);

  /* The second argument of remquo/remainder/drem must not be 0.  */
  TESTIT_REMQUO (1.0, 0.0);
  TESTIT_REMQUO (1.0, -0.0);
  TESTIT2 (remainder, 1.0, 0.0);
  TESTIT2 (remainder, 1.0, -0.0);
  TESTIT2 (drem, 1.0, 0.0);
  TESTIT2 (drem, 1.0, -0.0);

  /* The argument to lgamma* cannot be zero or a negative integer.  */
  TESTIT_REENT (lgamma, -4.0); /* lgamma_r */
  TESTIT_REENT (lgamma, -3.0); /* lgamma_r */
  TESTIT_REENT (lgamma, -2.0); /* lgamma_r */
  TESTIT_REENT (lgamma, -1.0); /* lgamma_r */
  TESTIT_REENT (lgamma, -0.0); /* lgamma_r */
  TESTIT_REENT (lgamma, 0.0); /* lgamma_r */
  
  TESTIT_REENT (gamma, -4.0); /* gamma_r */
  TESTIT_REENT (gamma, -3.0); /* gamma_r */
  TESTIT_REENT (gamma, -2.0); /* gamma_r */
  TESTIT_REENT (gamma, -1.0); /* gamma_r */
  TESTIT_REENT (gamma, -0.0); /* gamma_r */
  TESTIT_REENT (gamma, 0.0); /* gamma_r */
}

/* { dg-final { scan-tree-dump-times "exp2 " 9 "original" } } */
/* { dg-final { scan-tree-dump-times "exp2f" 9 "original" { target { ! { spu*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "exp2f" 6 "original" { target { spu*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "exp2l" 9 "original" } } */
/* { dg-final { scan-tree-dump-times "asin " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "asinf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "asinl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "acos " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "acosf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "acosl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "acosh " 1 "original" } } */
/* { dg-final { scan-tree-dump-times "acoshf" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "acoshl" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "atanh " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "atanhf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "atanhl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "log " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "logf" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "logl" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log2 " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log2f" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log2l" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log10 " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log10f" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log10l" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "log1p " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "log1pf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "log1pl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "tgamma " 5 "original" } } */
/* { dg-final { scan-tree-dump-times "tgammaf" 5 "original" } } */
/* { dg-final { scan-tree-dump-times "tgammal" 5 "original" } } */
/* { dg-final { scan-tree-dump-times "pow " 13 "original" } } */
/* { dg-final { scan-tree-dump-times "powf" 13 "original" { target { ! { spu*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "powf" 7 "original" { target { spu*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "powl" 13 "original" } } */
/* { dg-final { scan-tree-dump-times "sqrt " 1 "original" } } */
/* { dg-final { scan-tree-dump-times "sqrtf" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "sqrtl" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "ldexp " 8 "original" } } */
/* { dg-final { scan-tree-dump-times "ldexpf" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "ldexpl" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalbn " 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalbnf" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalbnl" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalbln " 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalblnf" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "scalblnl" 8 "original" } } */
/* { dg-final { scan-tree-dump-times "_logb " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "_logbf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "_logbl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "ilogb " 6 "original" } } */
/* { dg-final { scan-tree-dump-times "ilogbf" 6 "original" { target { ! { spu*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "ilogbf" 2 "original" { target { spu*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "ilogbl" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "y0 " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "y0f" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "y0l" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "y1 " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "y1f" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "y1l" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "yn " 6 "original" } } */
/* { dg-final { scan-tree-dump-times "ynf" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "ynl" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "remquo " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "remquof" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "remquol" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "remainder " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "remainderf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "remainderl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "drem " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "dremf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "dreml" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "lgamma_r " 6 "original" } } */
/* { dg-final { scan-tree-dump-times "lgammaf_r" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "lgammal_r" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "_gamma_r " 6 "original" } } */
/* { dg-final { scan-tree-dump-times "_gammaf_r" 6 "original" } } */
/* { dg-final { scan-tree-dump-times "_gammal_r" 6 "original" } } */
