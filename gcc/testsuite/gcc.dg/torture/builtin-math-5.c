/* Copyright (C) 2009  Free Software Foundation.

   Test things that should block GCC from optimizing compile-time
   constants passed to a builtin complex transcendental functions.

   Origin: Kaveh R. Ghazi,  January 28, 2009.  */

/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

extern void foof (_Complex float);
extern void foo (_Complex double);
extern void fool (_Complex long double);

#define TESTIT(FUNC, ARG) do { \
  foof (__builtin_##FUNC##f (ARG##F)); \
  foo (__builtin_##FUNC (ARG)); \
  fool (__builtin_##FUNC##l (ARG##L)); \
} while (0)

#define TESTIT2(FUNC, ARG0, ARG1) do { \
  foof (__builtin_##FUNC##f (ARG0##F, ARG1##F)); \
  foo (__builtin_##FUNC (ARG0, ARG1)); \
  fool (__builtin_##FUNC##l (ARG0##L, ARG1##L)); \
} while (0)

void bar()
{
  /* An argument of NaN is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_csqrtf (__builtin_nanf("")));
#endif
  foo (__builtin_csqrt (__builtin_nan("")));
  fool (__builtin_csqrtl (__builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_csqrtf (__builtin_inff()));
#endif
  foo (__builtin_csqrt (__builtin_inf()));
  fool (__builtin_csqrtl (__builtin_infl()));
#ifndef __SPU__
  foof (__builtin_csqrtf (-__builtin_inff()));
#endif
  foo (__builtin_csqrt (-__builtin_inf()));
  fool (__builtin_csqrtl (-__builtin_infl()));

  /* Check for overflow/underflow.  */
  TESTIT (cexp, 1e20);
  TESTIT (cexp, -1e20);
  
  /* An argument of NaN is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_cpowf (__builtin_nanf(""), 2.5F));
#endif
  foo (__builtin_cpow (__builtin_nan(""), 2.5));
  fool (__builtin_cpowl (__builtin_nanl(""), 2.5L));
#ifndef __SPU__
  foof (__builtin_cpowf (2.5F, __builtin_nanf("")));
#endif
  foo (__builtin_cpow (2.5, __builtin_nan("")));
  fool (__builtin_cpowl (2.5L, __builtin_nanl("")));

  /* An argument of Inf/-Inf is not evaluated at compile-time.  */
#ifndef __SPU__
  foof (__builtin_cpowf (__builtin_inff(), 2.5F));
#endif
  foo (__builtin_cpow (__builtin_inf(), 2.5));
  fool (__builtin_cpowl (__builtin_infl(), 2.5L));
#ifndef __SPU__
  foof (__builtin_cpowf (-__builtin_inff(), 2.5F));
#endif
  foo (__builtin_cpow (-__builtin_inf(), 2.5));
  fool (__builtin_cpowl (-__builtin_infl(), 2.5L));
#ifndef __SPU__
  foof (__builtin_cpowf (2.5F, __builtin_inff()));
#endif
  foo (__builtin_cpow (2.5, __builtin_inf()));
  fool (__builtin_cpowl (2.5L, __builtin_infl()));
#ifndef __SPU__
  foof (__builtin_cpowf (2.5F, -__builtin_inff()));
#endif
  foo (__builtin_cpow (2.5, -__builtin_inf()));
  fool (__builtin_cpowl (2.5L, -__builtin_infl()));

  /* Check for Inv/NaN return values.  */
  TESTIT2 (cpow, -0.0, -4.5); /* Returns Inf */
  TESTIT2 (cpow, 0.0, -4.5); /* Returns Inf */

  /* Check for overflow/underflow.  */
  foof (__builtin_cpowf (__FLT_MAX__, 3.5F));
  foof (__builtin_cpowf (__FLT_MAX__ * 1.FI, 3.5F));
  foo (__builtin_cpow (__DBL_MAX__, 3.5));
  foo (__builtin_cpow (__DBL_MAX__ * 1.I, 3.5));
  fool (__builtin_cpowl (__LDBL_MAX__, 3.5L));
  fool (__builtin_cpowl (__LDBL_MAX__ * 1.LI, 3.5L));
  TESTIT2 (cpow, 2.0, 0x1p50);
  TESTIT2 (cpow, 2.0, 0x1p28);
  TESTIT2 (cpow, 2.0, 0x1p24);
  foof (__builtin_cpowf (__FLT_MAX__, -3.5F));
  foof (__builtin_cpowf (__FLT_MAX__ * 1.FI, -3.5F));
  foo (__builtin_cpow (__DBL_MAX__, -3.5));
  foo (__builtin_cpow (__DBL_MAX__ * 1.I, -3.5));
  fool (__builtin_cpowl (__LDBL_MAX__, -3.5L));
  fool (__builtin_cpowl (__LDBL_MAX__ * 1.LI, -3.5L));
  TESTIT2 (cpow, 2.0, -0x1p50);
  TESTIT2 (cpow, 2.0, -0x1p28);
  TESTIT2 (cpow, 2.0, -0x1p24);

}

/* { dg-final { scan-tree-dump-times "csqrtf" 3 "original" { target { ! { spu*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "csqrtf" 0 "original" { target { spu*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "csqrt " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "csqrtl" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "cexpf" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "cexp " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "cexpl" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "cpowf" 18 "original" { target { ! { spu*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "cpowf" 12 "original" { target { spu*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "cpow " 18 "original" } } */
/* { dg-final { scan-tree-dump-times "cpowl" 18 "original" } } */
