/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vst2_lane_bf16:
**	vst2.16	{d0\[2\], d1\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst2_lane_bf16 (bfloat16_t *a, bfloat16x4x2_t b)
{
  return vst2_lane_bf16 (a, b, 2);
}

/*
**test_vst2q_lane_bf16:
**	vst2.16	{d0\[2\], d2\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst2q_lane_bf16 (bfloat16_t *a, bfloat16x8x2_t b)
{
  return vst2q_lane_bf16 (a, b, 2);
}

/*
**test_vst3_lane_bf16:
**	vst3.16	{d0\[2\], d1\[2\], d2\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst3_lane_bf16 (bfloat16_t *a, bfloat16x4x3_t b)
{
  return vst3_lane_bf16 (a, b, 2);
}

/*
**test_vst3q_lane_bf16:
**	vst3.16	{d0\[2\], d2\[2\], d4\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst3q_lane_bf16 (bfloat16_t *a, bfloat16x8x3_t b)
{
  return vst3q_lane_bf16 (a, b, 2);
}

/*
**test_vst4_lane_bf16:
**	vst4.16	{d0\[2\], d1\[2\], d2\[2\], d3\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst4_lane_bf16 (bfloat16_t *a, bfloat16x4x4_t b)
{
  return vst4_lane_bf16 (a, b, 2);
}

/*
**test_vst4q_lane_bf16:
**	vst4.16	{d0\[2\], d2\[2\], d4\[2\], d6\[2\]}, \[r0\]
**	bx	lr
*/
void
test_vst4q_lane_bf16 (bfloat16_t *a, bfloat16x8x4_t b)
{
  return vst4q_lane_bf16 (a, b, 2);
}
