/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_2.c"
#include "abitest.h"

#else
  ARG(float, 1.0f, S0)
  ARG(double, 4.0, D1)
  ARG(float, 2.0f, S2)
  ARG(double, 5.0, D3)
  ARG(__fp16, 8.0f, H4)
  LAST_ARG(int, 3, W0)
#endif
