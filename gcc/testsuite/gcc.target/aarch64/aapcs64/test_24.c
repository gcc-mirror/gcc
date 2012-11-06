/* Test AAPCS64 layout.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_24.c"

typedef long double TFtype;

#include "abitest.h"
#else
  ARG(TFtype, 1.0, Q0)
  ARG(TFtype, 2.0, Q1)
  ARG(TFtype, 3.0, Q2)
  ARG(TFtype, 4.0, Q3)
  ARG(TFtype, 5.0, Q4)
  ARG(TFtype, 6.0, Q5)
  ARG(TFtype, 7.0, Q6)
  ARG(TFtype, 8.0, Q7)
  ARG(double, 9.0, STACK)
  LAST_ARG(TFtype, 10.0, STACK+16)
#endif
