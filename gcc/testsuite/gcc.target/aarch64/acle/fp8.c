/* Test the fp8 ACLE intrinsics family.  */
/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a" } */

#include <arm_acle.h>

#ifdef __ARM_FEATURE_FP8
#error "__ARM_FEATURE_FP8 feature macro defined."
#endif

#pragma GCC push_options
#pragma GCC target("arch=armv9.4-a+fp8")

/* We do not define __ARM_FEATURE_FP8 until all
   relevant features have been added. */
#ifdef __ARM_FEATURE_FP8
#error "__ARM_FEATURE_FP8 feature macro defined."
#endif

#pragma GCC pop_options
