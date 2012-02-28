/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-*eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp1.c"
#include "abitest.h"

#else
  ARG(int, 4, R0)
  ARG(double, 4.0, D0)
  LAST_ARG(int, 3, R1)
#endif
