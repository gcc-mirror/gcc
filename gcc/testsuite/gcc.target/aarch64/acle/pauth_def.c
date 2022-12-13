/* { dg-do compile } */

#pragma GCC target "+pauth"
#ifndef __ARM_FEATURE_PAUTH
#error "__ARM_FEATURE_PAUTH is not defined but should be!"
#endif

void
foo (void) {}

