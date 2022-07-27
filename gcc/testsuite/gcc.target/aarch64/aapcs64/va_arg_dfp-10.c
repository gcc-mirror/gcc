/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: Anonymous arguments passed on the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-10.c"

struct z
{
  _Decimal64 x[4];
};

_Decimal64 d1 = 25.0dd;
_Decimal64 d2 = 103.0dd;
struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#include "abitest.h"
#else
  ARG(struct z, a, D0, 0)
  ARG(struct z, b, D4, LAST_NAMED_ARG_ID)
  DOTS
  ANON(_Decimal64, d1, STACK, 2)
  LAST_ANON(_Decimal64, d2, STACK+8, 3)

#endif
