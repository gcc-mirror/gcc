/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only and without Neon" { *-*-* } { "-mpure-code" } } */
/* { dg-require-effective-target arm_neon_h_ok } */
/* { dg-options "-mfp16-format=ieee" } */
/* { dg-add-options arm_neon_h } */

#include "arm_neon.h"

float16x4_t __attribute__((target("fpu=neon-fp16")))
foo (float32x4_t arg)
{
    return vcvt_f16_f32 (arg);
}
