/* Test AAPCS64 layout.

   Test parameter passing of floating-point quad precision types.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_quad_double_dfp.c"

typedef _Decimal128 TDtype;
typedef _Complex long double CTFtype;

TDtype x = 1.0dl;
TDtype y = 2.0dl;

CTFtype cx = 3.0 + 4.0i;
CTFtype cy = 5.0 + 6.0i;

#include "abitest.h"
#else
  ARG       ( TDtype,  x, Q0)
  ARG       (CTFtype, cx, Q1)
  DOTS
  ANON      (CTFtype, cy, Q3)
  LAST_ANON ( TDtype,  y, Q5)
#endif
