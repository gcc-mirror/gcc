/* Test AAPCS64 layout.

   Test parameter passing of floating-point quad precision types.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_quad_double.c"

typedef long double TFtype;
typedef _Complex long double CTFtype;

TFtype x = 1.0;
TFtype y = 2.0;

CTFtype cx = 3.0 + 4.0i;
CTFtype cy = 5.0 + 6.0i;

#include "abitest.h"
#else
  ARG       ( TFtype,  x, Q0)
  ARG       (CTFtype, cx, Q1)
  DOTS
  ANON      (CTFtype, cy, Q3)
  LAST_ANON ( TFtype,  y, Q5)
#endif
