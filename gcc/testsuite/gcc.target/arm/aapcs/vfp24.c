/* Test AAPCS layout (VFP variant)  */

/* { dg-do run { target arm_eabi } }  */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_fp16_hw }  */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-add-options arm_fp16_alternative }  */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp24.c"

#define PCSATTR __attribute__((pcs("aapcs")))

#include "abitest.h"
#else
ARG (float, 1.0f, R0)
ARG (double, 2.0, R2)
ARG (float, 3.0f, STACK)
ARG (__fp16, 2.0f, STACK+4)
LAST_ARG (double, 4.0, STACK+8)
#endif
