/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile without Neon only" { *-*-* } { "-mpure-code" } } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-require-effective-target arm_libc_fp_abi_ok } */
/* { dg-options "-mfp16-format=ieee -mfpu=auto" } */
/* { dg-add-options arm_arch_v7a } */
/* { dg-add-options arm_libc_fp_abi } */

#include "arm_neon.h"

float16x4_t __attribute__((target("fpu=neon-fp16")))
foo (float32x4_t arg)
{
    return vcvt_f16_f32 (arg);
}
