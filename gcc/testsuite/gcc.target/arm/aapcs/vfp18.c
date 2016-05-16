/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm_eabi } }  */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard -mfp16-format=ieee" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp18.c"
#include "abitest.h"

#else
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 1.0f, S0 + 2)
#else
ARG (__fp16, 1.0f, S0)
#endif
ARG (float, 2.0f, S1)
ARG (double, 4.0, D1)
ARG (float, 2.0f, S4)
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 1.0f, S5 + 2)
#else
ARG (__fp16, 1.0f, S5)
#endif
LAST_ARG (int, 3, R0)
#endif
