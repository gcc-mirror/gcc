/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_20.c"

#include "abitest.h"

#else
  ARG(int, 8, W0)
  ARG(_Decimal64, 1.0dd, D0)
  ARG(_Decimal64, 2.0dd, D1)
  ARG(_Decimal64, 3.0dd, D2)
  ARG(_Decimal64, 4.0dd, D3)
  ARG(_Decimal64, 5.0dd, D4)
  ARG(_Decimal64, 6.0dd, D5)
  ARG(_Decimal64, 7.0dd, D6)
  DOTS
  ANON(_Complex double, 1234.0 + 567.0i, STACK)
  LAST_ANON(_Decimal64, -987.0dd, STACK+16)
#endif
