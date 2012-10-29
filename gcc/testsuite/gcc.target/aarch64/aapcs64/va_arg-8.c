/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: HFA anonymous parameter passed in SIMD/FP regs.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-8.c"

struct z
{
  double x[4];
};

struct z a = { 5.0, 6.0, 7.0, 8.0 };

#include "abitest.h"
#else
  ARG(int, 0xdeadbeef, W0, LAST_NAMED_ARG_ID)
  DOTS
  ANON(double, 4.0, D0, 1)
  LAST_ANON(struct z, a, D1, 2)

#endif
