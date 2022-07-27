/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_15.c"

#include "abitest.h"
#else
  ARG(_Decimal64, 1.0dd, D0)
  ARG(_Decimal64, 2.0dd, D1)
  ARG(_Decimal64, 3.0dd, D2)
  ARG(_Decimal64, 4.0dd, D3)
  ARG(_Decimal64, 5.0dd, D4)
  ARG(_Decimal64, 6.0dd, D5)
  ARG(_Decimal64, 7.0dd, D6)
  ARG(_Decimal64, 8.0dd, D7)
  ARG(_Decimal64, 9.0dd, STACK)
  LAST_ARG(_Decimal64, 10.0dd, STACK+8)
#endif
