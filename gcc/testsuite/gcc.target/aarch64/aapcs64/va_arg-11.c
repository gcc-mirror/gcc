/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: Anonymous arguments passed on the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-11.c"

struct z
{
  double x[2];
};

double d1 = 25.0;
struct z a = { 5.0, 6.0 };

#include "abitest.h"
#else
  ARG(double, 1.0, D0, 0)
  ARG(double, 2.0, D1, 1)
  ARG(double, 3.0, D2, 2)
  ARG(double, 4.0, D3, 3)
  ARG(double, 5.0, D4, 4)
  ARG(double, 6.0, D5, 5)
  ARG(double, 7.0, D6, LAST_NAMED_ARG_ID)
  DOTS
  ANON(struct z, a, STACK, 8)
  LAST_ANON(double, d1, STACK+16, 9)

#endif
