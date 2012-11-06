/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_4.c"

__complex__ float x = 1.0f + 2.0fi;
#include "abitest.h"
#else
ARG (float, 1.0f, S0)
ARG (__complex__ float, x, S1)
ARG (float, 2.0f, S3)
ARG (double, 5.0, D4)
LAST_ARG_NONFLAT (int, 3, X0, i32in64)
#endif
