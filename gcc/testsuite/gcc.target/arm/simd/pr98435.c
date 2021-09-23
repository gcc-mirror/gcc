/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-require-effective-target arm_softfp_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-mfloat-abi=softfp -march=armv8.2-a+bf16+fp16" } */

#include <arm_neon.h>

bfloat16x4_t f (bfloat16_t a)
{
  return (bfloat16x4_t) {a, a, a, a};
}

/* { dg-final { scan-assembler {\tvdup.16\td[0-9]+, r0} } } */
/* { dg-final { scan-assembler {\tvmov\tr0, r1, d[0-9]+} } } */
