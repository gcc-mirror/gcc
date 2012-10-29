/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_6.c"

__complex__ double x = 1.0+2.0i;

struct y
{
  int p;
  int q;
  int r;
  int s;
} v = { 1, 2, 3, 4 };

#include "abitest.h"
#else
  ARG(struct y, v, X0)
  ARG(float, 1.0f, S0)
  ARG(__complex__ double, x, D1)
  ARG(float, 2.0f, S3)
  ARG(double, 5.0, D4)
  LAST_ARG(int, 3, W2)
#endif
