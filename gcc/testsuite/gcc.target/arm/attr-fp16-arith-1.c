/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_2a_fp16_scalar } */

/* Reset fpu to a value compatible with the next pragmas.  */
#pragma GCC target ("fpu=vfp")

#pragma GCC push_options
#pragma GCC target ("fpu=fp-armv8")

#ifndef __ARM_FEATURE_FP16_SCALAR_ARITHMETIC
#error __ARM_FEATURE_FP16_SCALAR_ARITHMETIC not defined.
#endif

#pragma GCC push_options
#pragma GCC target ("fpu=neon-fp-armv8")

#ifndef __ARM_FEATURE_FP16_VECTOR_ARITHMETIC
#error __ARM_FEATURE_FP16_VECTOR_ARITHMETIC not defined.
#endif

#ifndef __ARM_NEON
#error __ARM_NEON not defined.
#endif

#if !defined (__ARM_FP) || !(__ARM_FP & 0x2)
#error Invalid value for __ARM_FP
#endif

#include "arm_neon.h"

float16_t
foo (float16x4_t b)
{
  float16x4_t a = {2.0, 3.0, 4.0, 5.0};
  float16x4_t res = vadd_f16 (a, b);

  return res[0];
}

/* { dg-final { scan-assembler "vadd\\.f16\td\[0-9\]+, d\[0-9\]+" } } */

#pragma GCC pop_options

/* Check that the FP version is correctly reset to mfpu=fp-armv8.  */

#if !defined (__ARM_FP) || !(__ARM_FP & 0x2)
#error __ARM_FP should record FP16 support.
#endif

#pragma GCC pop_options

/* Check that the FP version is correctly reset to mfpu=vfp.  */

#if !defined (__ARM_FP) || (__ARM_FP & 0x2)
#error Unexpected value for __ARM_FP.
#endif
