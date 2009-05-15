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
}

/* { dg-final { scan-tree-dump-times "csqrtf" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "csqrt " 3 "original" } } */
/* { dg-final { scan-tree-dump-times "csqrtl" 3 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
