/* Test the cdp2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc2_ok } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x2) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_cdp2 (void)
{
  __arm_cdp2 (10, 4, 3, 2, 1, 0);
}

/* { dg-final { scan-assembler "cdp2\tp10, #4, CR3, CR2, CR1, #0\n" } } */
