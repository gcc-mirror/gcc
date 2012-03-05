/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-*eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp3.c"

__complex__ x = 1.0+2.0i;

#include "abitest.h"
#else
  ARG(float, 1.0f, S0)
  ARG(__complex__ double, x, D1)
  ARG(float, 2.0f, S1)
  ARG(double, 5.0, D3)
  LAST_ARG(int, 3, R0)
#endif
