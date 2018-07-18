/* Test the cdp ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc1_ok } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x1) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_cdp (void)
{
  __arm_cdp (10, 1, 2, 3, 4, 5);
}

/* { dg-final { scan-assembler "cdp\tp10, #1, CR2, CR3, CR4, #5\n" } } */
