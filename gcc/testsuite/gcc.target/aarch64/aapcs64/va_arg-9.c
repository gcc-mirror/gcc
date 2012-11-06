/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: HFA anonymous parameter passed in SIMD/FP regs.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-9.c"

struct z
{
  double x[4];
};

double d1 = 25.0;
struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else
  ARG(double, 11.0, D0, LAST_NAMED_ARG_ID)
  DOTS
  ANON(int, 8, W0, 1)
  ANON(struct z, a, D1, 2)
  ANON(struct z, b, STACK, 3)
  ANON(int, 5, W1, 4)
  ANON(double, d1, STACK+32, 5)
  LAST_ANON(double, 0.5, STACK+40, 6)

#endif
