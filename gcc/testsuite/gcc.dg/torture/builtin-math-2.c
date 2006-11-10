/* Copyright (C) 2006  Free Software Foundation.

   Test things that should block GCC from optimizing compile-time
   constants passed to a builtin transcendental function.

   Origin: Kaveh R. Ghazi,  October 22, 2006.  */

/* { dg-do compile } */
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

void bar()
{
  /* An argument of NaN is not evaluated at compile-time.  */
  foof (__builtin_exp2f (__builtin_nanf("")));
  foo (__builtin_exp2 (__builtin_nan("")));
  fool (__builtin_exp2l (__builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
  foof (__builtin_exp2f (__builtin_inff()));
  foo (__builtin_exp2 (__builtin_inf()));
  fool (__builtin_exp2l (__builtin_infl()));
  foof (__builtin_exp2f (-__builtin_inff()));
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
  foof (__builtin_powf (__builtin_nanf(""), 2.5F));
  foo (__builtin_pow (__builtin_nan(""), 2.5));
  fool (__builtin_powl (__builtin_nanl(""), 2.5L));
  foof (__builtin_powf (2.5F, __builtin_nanf("")));
  foo (__builtin_pow (2.5, __builtin_nan("")));
  fool (__builtin_powl (2.5L, __builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
  foof (__builtin_powf (__builtin_inff(), 2.5F));
  foo (__builtin_pow (__builtin_inf(), 2.5));
  fool (__builtin_powl (__builtin_infl(), 2.5L));
  foof (__builtin_powf (-__builtin_inff(), 2.5F));
  foo (__builtin_pow (-__builtin_inf(), 2.5));
  fool (__builtin_powl (-__builtin_infl(), 2.5L));
  foof (__builtin_powf (2.5F, __builtin_inff()));
  foo (__builtin_pow (2.5, __builtin_inf()));
  fool (__builtin_powl (2.5L, __builtin_infl()));
  foof (__builtin_powf (2.5F, -__builtin_inff()));
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
  
  foof (__builtin_fmaf (__FLT_MAX__, __FLT_MAX__, 0.0F));
  foof (__builtin_fmaf (__FLT_MAX__, 1.0F, __FLT_MAX__));
  foof (__builtin_fmaf (__FLT_MIN__, __FLT_MIN__, 0.0F));
  
  foo (__builtin_fma (__DBL_MAX__, __DBL_MAX__, 0.0));
  foo (__builtin_fma (__DBL_MAX__, 1.0, __DBL_MAX__));
  foo (__builtin_fma (__DBL_MIN__, __DBL_MIN__, 0.0));
  
  fool (__builtin_fmal (__LDBL_MAX__, __LDBL_MAX__, 0.0L));
  fool (__builtin_fmal (__LDBL_MAX__, 1.0L, __LDBL_MAX__));
  fool (__builtin_fmal (__LDBL_MIN__, __LDBL_MIN__, 0.0L));
}

/* { dg-final { scan-tree-dump-times "exp2 " 9 "original" } } */
/* { dg-final { scan-tree-dump-times "exp2f" 9 "original" } } */
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
/* { dg-final { scan-tree-dump-times "powf" 13 "original" } } */
/* { dg-final { scan-tree-dump-times "powl" 13 "original" } } */
/* { dg-final { scan-tree-dump-times "fma " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "fmaf" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "fmal" 3 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
