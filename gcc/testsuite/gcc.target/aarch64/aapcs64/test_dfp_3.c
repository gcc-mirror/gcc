/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_3.c"

__complex__ x = 1.0+2.0i;

#include "abitest.h"
#else
ARG (_Decimal32, 1.0df, S0)
ARG (__complex__ double, x, D1)
ARG (_Decimal32, 2.0df, S3)
ARG (_Decimal64, 5.0dd, D4)
LAST_ARG_NONFLAT (int, 3, X0, i32in64)
#endif
