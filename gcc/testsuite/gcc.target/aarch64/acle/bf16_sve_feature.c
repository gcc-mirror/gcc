/* { dg-do compile } */

#pragma GCC target "+sve+bf16"
#ifndef __ARM_FEATURE_SVE_BF16
#error "__ARM_FEATURE_SVE_BF16 is not defined but should be!"
#endif

void
foo (void) {}

