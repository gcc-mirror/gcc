/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_5.c"

__complex__ float x = 1.0+2.0i;

struct y
{
  long p;
  long q;
} v = { 1, 2};

#include "abitest.h"
#else
  ARG(_Decimal32, 1.0df, S0)
  ARG(__complex__ float, x, S1)
  ARG(_Decimal32, 2.0df, S3)
  ARG(_Decimal64, 5.0dd, D4)
  LAST_ARG(struct y, v, X0)
#endif
