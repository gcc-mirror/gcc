/* { dg-do assemble } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vfmabq_f32:
**      ...
**      vfmab.bf16	q0, q1, q2
**      bx	lr
*/
float32x4_t
test_vfmabq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlalbq_f32 (r, a, b);
}

/*
**test_vfmatq_f32:
**      ...
**      vfmat.bf16	q0, q1, q2
**      bx	lr
*/
float32x4_t
test_vfmatq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlaltq_f32 (r, a, b);
}

/*
**test_vfmabq_lane_f32:
**      ...
**      vfmab.bf16	q0, q1, d4\[0\]
**      bx	lr
*/
float32x4_t
test_vfmabq_lane_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  return vbfmlalbq_lane_f32 (r, a, b, 0);
}

/*
**test_vfmatq_lane_f32:
**      ...
**      vfmat.bf16	q0, q1, d4\[2\]
**      bx	lr
*/
float32x4_t
test_vfmatq_lane_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  return vbfmlaltq_lane_f32 (r, a, b, 2);
}

/*
**test_vfmabq_laneq_f32:
**      ...
**      vfmab.bf16	q0, q1, d5\[1\]
**      bx	lr
*/
float32x4_t
test_vfmabq_laneq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlalbq_laneq_f32 (r, a, b, 5);
}

/*
**test_vfmatq_laneq_f32:
**      ...
**      vfmat.bf16	q0, q1, d5\[3\]
**      bx	lr
*/
float32x4_t
test_vfmatq_laneq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlaltq_laneq_f32 (r, a, b, 7);
}
