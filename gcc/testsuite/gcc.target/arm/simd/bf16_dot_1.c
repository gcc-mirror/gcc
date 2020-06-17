/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

/* BF16 DOT without lane.  */
float32x2_t
test_vbfdot_f32 (float32x2_t r, bfloat16x4_t a, bfloat16x4_t b)
{
  /* vdot.bf16 d, d, d */
  return vbfdot_f32 (r, a, b);
}

float32x4_t
test_vbfdotq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* vdot.bf16 q, q, q */
  return vbfdotq_f32 (r, a, b);
}

/* 64-bit BF16 DOT with lane.  */
float32x2_t
test_vbfdot_lane_f32_0 (float32x2_t r, bfloat16x4_t a, bfloat16x4_t b)
{
  /* vdot.bf16 d, d, d[0] */
  return vbfdot_lane_f32 (r, a, b, 0);
}

float32x2_t
test_vbfdot_lane_f32_1 (float32x2_t r, bfloat16x4_t a, bfloat16x4_t b)
{
  /* vdot.bf16 d, d, d[1] */
  return vbfdot_lane_f32 (r, a, b, 1);
}

float32x2_t
test_vbfdot_laneq_f32_0 (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* vdot.bf16 d, d, d[0] */
  return vbfdot_laneq_f32 (r, a, b, 0);
}

float32x2_t
test_vbfdot_laneq_f32_1 (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* vdot.bf16 d, d, d[1] */
  return vbfdot_laneq_f32 (r, a, b, 1);
}

float32x2_t
test_vbfdot_laneq_f32_2 (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* vdot.bf16 d, d, d[0] */
  return vbfdot_laneq_f32 (r, a, b, 2);
}

float32x2_t
test_vbfdot_laneq_f32_3 (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* vdot.bf16 d, d, d[1] */
  return vbfdot_laneq_f32 (r, a, b, 3);
}

/* 128-bit BF16 DOT with lane.  */
float32x4_t
test_vbfdotq_lane_f32_0 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* vdot.bf16 q, q, d[0] */
  return vbfdotq_lane_f32 (r, a, b, 0);
}

float32x4_t
test_vbfdotq_lane_f32_1 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* vdot.bf16 q, q, d[1] */
  return vbfdotq_lane_f32 (r, a, b, 1);
}

float32x4_t
test_vbfdotq_laneq_f32_0 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* vdot.bf16 q, q, d[0] */
  return vbfdotq_laneq_f32 (r, a, b, 0);
}

float32x4_t
test_vbfdotq_laneq_f32_3 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* vdot.bf16 q, q, d[1] */
  return vbfdotq_laneq_f32 (r, a, b, 3);
}

/* { dg-final { scan-assembler-times {\tvdot.bf16\td[0-9]+, d[0-9]+, d[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvdot.bf16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvdot.bf16\td[0-9]+, d[0-9]+, d[0-9]+\[0\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvdot.bf16\td[0-9]+, d[0-9]+, d[0-9]+\[1\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvdot.bf16\tq[0-9]+, q[0-9]+, d[0-9]+\[0\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvdot.bf16\tq[0-9]+, q[0-9]+, d[0-9]+\[1\]\n} 2 } } */
