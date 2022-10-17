/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: HFA anonymous parameter passed in SIMD/FP regs.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-8.c"

struct z
{
  _Decimal64 x[4];
};

struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };

#include "abitest.h"
#else
  ARG(int, 0xdeadbeef, W0, LAST_NAMED_ARG_ID)
  DOTS
  ANON(_Decimal64, 4.0dd, D0, 1)
  LAST_ANON(struct z, a, D1, 2)

#endif
