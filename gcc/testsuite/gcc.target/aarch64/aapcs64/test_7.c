/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_7.c"

__complex__ float x = 1.0f + 2.0i;

struct y
{
  int p;
  int q;
  int r;
  int s;
} v = { 1, 2, 3, 4 }, v1 = {5, 6, 7, 8}, v2 = {9, 10, 11, 12};

#include "abitest.h"
#else
ARG (struct y, v, X0)
ARG (struct y, v1, X2)
ARG (struct y, v2, X4)
ARG (int, 4, W6)
ARG (float, 1.0f, S0)
ARG (__complex__ float, x, S1)
ARG (float, 2.0f, S3)
ARG (double, 5.0, D4)
ARG (int, 3, W7)
LAST_ARG_NONFLAT (int, 5, STACK, i32in64)
#endif
