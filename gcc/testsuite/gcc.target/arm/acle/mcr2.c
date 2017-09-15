/* Test the mcr2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc2_ok } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x2) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_mcr2 (uint32_t a)
{
  a += 77;
  __arm_mcr2 (10, 5, a, 3, 4, 7);
}

/* { dg-final { scan-assembler "add\[^\n\]*#77\n" } } */
/* { dg-final { scan-assembler "mcr2\tp10, #5, r\[r0-9\]*, CR3, CR4, #7\n" } } */
