/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-mfp16-format=ieee -mfloat-abi=softfp" } */

#include "arm_neon.h"

float16x4_t __attribute__((target("fpu=neon-fp16")))
foo (float32x4_t arg)
{
    return vcvt_f16_f32 (arg);
}
