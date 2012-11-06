/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_15.c"

#include "abitest.h"
#else
  ARG(double, 1.0, D0)
  ARG(double, 2.0, D1)
  ARG(double, 3.0, D2)
  ARG(double, 4.0, D3)
  ARG(double, 5.0, D4)
  ARG(double, 6.0, D5)
  ARG(double, 7.0, D6)
  ARG(double, 8.0, D7)
  ARG(double, 9.0, STACK)
  LAST_ARG(double, 10.0, STACK+8)
#endif
