/* Test AAPCS layout (VFP variant)  */

/* { dg-do run { target arm_eabi } }  */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_fp16_hw }  */
/* { dg-add-options arm_fp16_ieee }  */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp19.c"

__complex__ x = 1.0+2.0i;

#include "abitest.h"
#else
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 1.0f, S0 + 2)
#else
ARG (__fp16, 1.0f, S0)
#endif
ARG (float, 2.0f, S1)
ARG (__complex__ double, x, D1)
ARG (float, 3.0f, S6)
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 2.0f, S7 + 2)
#else
ARG (__fp16, 2.0f, S7)
#endif
LAST_ARG (int, 3, R0)
#endif
