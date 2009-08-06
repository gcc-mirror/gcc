/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp17.c"

#define PCSATTR __attribute__((pcs("aapcs")))

#include "abitest.h"
#else
  ARG(float, 1.0f, R0)
  ARG(double, 2.0, R2)
  ARG(float, 3.0f, STACK)
  LAST_ARG(double, 4.0, STACK+8)
#endif
