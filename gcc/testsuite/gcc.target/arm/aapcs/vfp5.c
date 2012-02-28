/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm*-*-*eabi* } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp5.c"

__complex__ float x = 1.0+2.0i;

struct y
{
  int p;
  int q;
  int r;
  int s;
} v = { 1, 2, 3, 4 };

#include "abitest.h"
#else
  ARG(float, 1.0f, S0)
  ARG(__complex__ float, x, S1)
  ARG(float, 2.0f, S3)
  ARG(double, 5.0, D2)
  ARG(struct y, v, R0)
  LAST_ARG(int, 3, STACK)
#endif
