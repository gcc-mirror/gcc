/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: Anonymous arguments passed on the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-11.c"

struct z
{
  _Decimal64 x[2];
};

_Decimal64 d1 = 25.0dd;
struct z a = { 5.0dd, 6.0dd };

#include "abitest.h"
#else
  ARG(_Decimal64, 1.0dd, D0, 0)
  ARG(_Decimal64, 2.0dd, D1, 1)
  ARG(_Decimal64, 3.0dd, D2, 2)
  ARG(_Decimal64, 4.0dd, D3, 3)
  ARG(_Decimal64, 5.0dd, D4, 4)
  ARG(_Decimal64, 6.0dd, D5, 5)
  ARG(_Decimal64, 7.0dd, D6, LAST_NAMED_ARG_ID)
  DOTS
  ANON(struct z, a, STACK, 8)
  LAST_ANON(_Decimal64, d1, STACK+16, 9)

#endif
