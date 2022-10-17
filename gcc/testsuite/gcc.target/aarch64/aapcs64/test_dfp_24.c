/* Test AAPCS64 layout.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_24.c"

typedef _Decimal128 TDtype;

#include "abitest.h"
#else
  ARG(TDtype, 1.0dl, Q0)
  ARG(TDtype, 2.0dl, Q1)
  ARG(TDtype, 3.0dl, Q2)
  ARG(TDtype, 4.0dl, Q3)
  ARG(TDtype, 5.0dl, Q4)
  ARG(TDtype, 6.0dl, Q5)
  ARG(TDtype, 7.0dl, Q6)
  ARG(TDtype, 8.0dl, Q7)
  ARG(_Decimal64, 9.0dd, STACK)
  LAST_ARG(TDtype, 10.0dl, STACK+16)
#endif
