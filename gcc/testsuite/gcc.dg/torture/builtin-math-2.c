/* Test things that should block GCC from optimizing compile-time
   constants passed to a builtin transcendental function.

   Origin: Kaveh R. Ghazi 10/22/2006.  */

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
}

/* { dg-final { scan-tree-dump-times "exp2 " 9 "original" } } */
/* { dg-final { scan-tree-dump-times "exp2f" 9 "original" } } */
/* { dg-final { scan-tree-dump-times "exp2l" 9 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
