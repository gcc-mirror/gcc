/* { dg-do compile } */

#pragma GCC target "+bf16"
#ifndef __ARM_FEATURE_BF16
#error "__ARM_FEATURE_BF16 is not defined but should be!"
#endif

void
foo (void) {}

