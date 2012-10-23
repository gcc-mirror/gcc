/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_3.c"

__complex__ x = 1.0+2.0i;

#include "abitest.h"
#else
ARG (float, 1.0f, S0)
ARG (__complex__ double, x, D1)
ARG (float, 2.0f, S3)
ARG (double, 5.0, D4)
LAST_ARG_NONFLAT (int, 3, X0, i32in64)
#endif
