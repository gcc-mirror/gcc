/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-*eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp4.c"

__complex__ float x = 1.0f + 2.0fi;
#include "abitest.h"
#else
  ARG(float, 1.0f, S0)
  ARG(__complex__ float, x, S1)
  ARG(float, 2.0f, S3)
  ARG(double, 5.0, D2)
  LAST_ARG(int, 3, R0)
#endif
