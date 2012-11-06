/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_complex.c"

__complex__ float x = 1.0+2.0i;
__complex__ int y = 5 + 6i;
__complex__ double z = 2.0 + 3.0i;

#include "abitest.h"
#else
  ARG(__complex__ float, x, S0)
  ARG(__complex__ int, y, X0)
  ARG(__complex__ double, z, D2)
  LAST_ARG (int, 5, W1)
#endif
