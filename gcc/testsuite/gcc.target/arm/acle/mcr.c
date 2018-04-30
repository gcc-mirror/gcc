/* Test the mcr ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc1_ok } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x1) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_mcr (uint32_t a)
{
  a += 77;
  __arm_mcr (10, 5, a, 3, 4, 7);
}

/* { dg-final { scan-assembler "add\[^\n\]*#77\n" } } */
/* { dg-final { scan-assembler "mcr\tp10, #5, r\[r0-9\]*, CR3, CR4, #7\n" } } */
