/* Test AAPCS64 layout and __builtin_va_arg.

   Miscellaneous test: Anonymous arguments passed on the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-10.c"

struct z
{
  double x[4];
};

double d1 = 25.0;
double d2 = 103.0;
struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else
  ARG(struct z, a, D0, 0)
  ARG(struct z, b, D4, LAST_NAMED_ARG_ID)
  DOTS
  ANON(double, d1, STACK, 2)
  LAST_ANON(double, d2, STACK+8, 3)

#endif
