/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp26.c"

/* Anonymous bitfields do not add members; if they do not change the layout
   then the end result may still be an HFA.  */
struct z
{
  float a;
  int :0;
  float b;
};

struct z a = { 5.0f, 6.0f };
struct z b = { 9.0f, 10.0f };

#define MYFUNCTYPE struct z

#include "abitest.h"
#else
  ARG(int, 7, R0)
  ARG(struct z, a, S0)
  LAST_ARG(struct z, b, S2)
#endif
