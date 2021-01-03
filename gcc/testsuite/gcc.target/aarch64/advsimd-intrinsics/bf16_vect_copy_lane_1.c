/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-additional-options "-march=armv8.2-a+bf16 -O3 --save-temps -std=gnu90" } */

#include "arm_neon.h"

bfloat16x4_t __attribute__((noinline,noclone))
test_vcopy_lane_bf16 (bfloat16x4_t a, bfloat16x4_t b)
{
  return vcopy_lane_bf16 (a, 1, b, 2);
}

bfloat16x8_t __attribute__((noinline,noclone))
test_vcopyq_lane_bf16 (bfloat16x8_t a, bfloat16x4_t b)
{
  return vcopyq_lane_bf16 (a, 1, b, 2);
}

bfloat16x4_t __attribute__((noinline,noclone))
test_vcopy_laneq_bf16 (bfloat16x4_t a, bfloat16x8_t b)
{
  return vcopy_laneq_bf16 (a, 1, b, 2);
}

bfloat16x8_t __attribute__((noinline,noclone))
test_vcopyq_laneq_bf16 (bfloat16x8_t a, bfloat16x8_t b)
{
  return vcopyq_laneq_bf16 (a, 1, b, 2);
}

/* { dg-final { scan-assembler-times "ins\\tv0.h\\\[1\\\], v1.h\\\[2\\\]" 2 } } */
/* { dg-final { scan-assembler-times "ins\\tv0.h\\\[1\\\], v1.h\\\[0\\\]" 2 } } */
