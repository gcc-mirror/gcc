/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_5.c"

__complex__ float x = 1.0+2.0i;

struct y
{
  long p;
  long q;
} v = { 1, 2};

#include "abitest.h"
#else
  ARG(float, 1.0f, S0)
  ARG(__complex__ float, x, S1)
  ARG(float, 2.0f, S3)
  ARG(double, 5.0, D4)
  LAST_ARG(struct y, v, X0)
#endif
