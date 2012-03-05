/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-*eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp16.c"

#define PCSATTR __attribute__((pcs("aapcs")))

#include "abitest.h"
#else
  ARG(float, 1.0f, R0)
  ARG(float, 2.0f, R1)
  ARG(float, 3.0f, R2)
  ARG(float, 4.0f, R3)
  ARG(float, 5.0f, STACK)
  LAST_ARG(float, 5.0f, STACK+4)
#endif
