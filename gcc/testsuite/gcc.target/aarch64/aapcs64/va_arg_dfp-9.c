/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: HFA anonymous parameter passed in SIMD/FP regs.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-9.c"

struct z
{
  _Decimal64 x[4];
};

_Decimal64 d1 = 25.0dd;
struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#include "abitest.h"
#else
  ARG(_Decimal64, 11.0dd, D0, LAST_NAMED_ARG_ID)
  DOTS
  ANON(int, 8, W0, 1)
  ANON(struct z, a, D1, 2)
  ANON(struct z, b, STACK, 3)
  ANON(int, 5, W1, 4)
  ANON(_Decimal64, d1, STACK+32, 5)
  LAST_ANON(_Decimal64, 0.5dd, STACK+40, 6)

#endif
