/* Copyright (C) 2006  Free Software Foundation.

   Test things that should block GCC from optimizing compile-time
   constants passed to a builtin transcendental function.

   Origin: Kaveh R. Ghazi,  October 22, 2006.  */

/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

extern void foof (float);
extern void foo (double);
extern void fool (long double);

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
  foof (__builtin_exp2f (0x1p50F));
  foo (__builtin_exp2 (0x1p50));
  fool (__builtin_exp2l (0x1p50L));
  /* Result underflows MPFR, which in version 2.2.x has 30 exponent bits.  */
  foof (__builtin_exp2f (-0x1p50F));
  foo (__builtin_exp2 (-0x1p50));
  fool (__builtin_exp2l (-0x1p50L));

  /* Result overflows GCC's REAL_VALUE_TYPE, which has 26 exponent bits.  */
  foof (__builtin_exp2f (0x1p28F));
  foo (__builtin_exp2 (0x1p28));
  fool (__builtin_exp2l (0x1p28L));
  /* Result underflows GCC's REAL_VALUE_TYPE, which has 26 exponent bits.  */
  foof (__builtin_exp2f (-0x1p28F));
  foo (__builtin_exp2 (-0x1p28));
  fool (__builtin_exp2l (-0x1p28L));

  /* Result overflows (even an extended) C double's mode.  */
  foof (__builtin_exp2f (0x1p24F));
  foo (__builtin_exp2 (0x1p24));
  fool (__builtin_exp2l (0x1p24L));
  /* Result underflows (even an extended) C double's mode.  */
  foof (__builtin_exp2f (-0x1p24F));
  foo (__builtin_exp2 (-0x1p24));
  fool (__builtin_exp2l (-0x1p24L));

  /* Ensure that normal arguments/results are folded.  */
  foof (__builtin_exp2f (1.5F));
  foo (__builtin_exp2 (1.5));
  fool (__builtin_exp2l (1.5L));
  foof (__builtin_exp2f (-1.5F));
  foo (__builtin_exp2 (-1.5));
  fool (__builtin_exp2l (-1.5L));

  /* The asin arg must be [-1 ... 1] inclusive.  */
  foof (__builtin_asinf (-1.5F));
  foof (__builtin_asinf (1.5F));
  foo (__builtin_asin (-1.5));
  foo (__builtin_asin (1.5));
  fool (__builtin_asinl (-1.5L));
  fool (__builtin_asinl (1.5L));

  /* The acos arg must be [-1 ... 1] inclusive.  */
  foof (__builtin_acosf (-1.5F));
  foof (__builtin_acosf (1.5F));
  foo (__builtin_acos (-1.5));
  foo (__builtin_acos (1.5));
  fool (__builtin_acosl (-1.5L));
  fool (__builtin_acosl (1.5L));

  /* The acosh arg must be [1 ... Inf] inclusive.  */
  foof (__builtin_acoshf (0.5F));
  foo (__builtin_acosh (0.5));
  fool (__builtin_acoshl (0.5L));

  /* The atanh arg must be [-1 ... 1] exclusive.  */
  foof (__builtin_atanhf (-1.0F));
  foof (__builtin_atanhf (1.0F));
  foo (__builtin_atanh (-1.0));
  foo (__builtin_atanh (1.0));
  fool (__builtin_atanhl (-1.0L));
  fool (__builtin_atanhl (1.0L));
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
/* { dg-final { cleanup-tree-dump "original" } } */
