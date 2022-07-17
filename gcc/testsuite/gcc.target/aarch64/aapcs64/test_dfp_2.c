/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_2.c"
#include "abitest.h"

#else
  ARG(_Decimal32, 1.0df, S0)
  ARG(_Decimal64, 4.0dd, D1)
  ARG(_Decimal32, 2.0df, S2)
  ARG(_Decimal64, 5.0dd, D3)
  ARG(__fp16, 8.0f, H4)
  LAST_ARG(int, 3, W0)
#endif
