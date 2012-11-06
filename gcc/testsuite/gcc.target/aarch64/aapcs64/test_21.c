/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_21.c"

#include "abitest.h"

#else
  ARG(int, 8, W0)
  ARG(double, 1.0, D0)
  ARG(double, 2.0, D1)
  ARG(double, 3.0, D2)
  ARG(double, 4.0, D3)
  ARG(double, 5.0, D4)
  ARG(double, 6.0, D5)
  ARG(double, 7.0, D6)
  ARG(_Complex double, 1234.0 + 567.0i, STACK)
  LAST_ARG(double, -987.0, STACK+16)
#endif
