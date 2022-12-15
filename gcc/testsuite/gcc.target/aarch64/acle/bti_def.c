/* { dg-do compile } */

#pragma GCC target "arch=armv8.5-a"
#ifndef __ARM_FEATURE_BTI
#error "__ARM_FEATURE_BTI is not defined but should be!"
#endif

void
foo (void) {}

