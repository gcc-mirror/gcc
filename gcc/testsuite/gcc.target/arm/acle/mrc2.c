/* Test the mrc2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc2_ok } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x2) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

uint32_t test_mrc2 (void)
{
  return __arm_mrc2 (10, 0, 0, 15, 3);
}

/* { dg-final { scan-assembler "mrc2\tp10, #0, r\[r0-9\]*, CR0, CR15, #3\n" } } */
