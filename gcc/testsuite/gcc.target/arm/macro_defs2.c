/* { dg-do compile } */
/* { dg-options "-march=armv7ve -mcpu=cortex-a15 -mfpu=neon-vfpv4" } */
/* { dg-add-options arm_neon } */
/* { dg-require-effective-target arm_neon_ok } */

#ifndef __ARM_NEON_FP
#error  __ARM_NEON_FP is not defined but should be
#endif

#ifndef __ARM_FP
#error  __ARM_FP is not defined but should be
#endif


