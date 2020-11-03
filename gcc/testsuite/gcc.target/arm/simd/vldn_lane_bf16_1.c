/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vld2_lane_bf16:
**	vld2.16	{d0\[2\], d1\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x4x2_t
test_vld2_lane_bf16 (const bfloat16_t *a, bfloat16x4x2_t b)
{
  return vld2_lane_bf16 (a, b, 2);
}

/*
**test_vld2q_lane_bf16:
**	vld2.16	{d0\[2\], d2\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x8x2_t
test_vld2q_lane_bf16 (const bfloat16_t *a, bfloat16x8x2_t b)
{
  return vld2q_lane_bf16 (a, b, 2);
}

/*
**test_vld3_lane_bf16:
**	vld3.16	{d0\[2\], d1\[2\], d2\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x4x3_t
test_vld3_lane_bf16 (const bfloat16_t *a, bfloat16x4x3_t b)
{
  return vld3_lane_bf16 (a, b, 2);
}

/*
**test_vld3q_lane_bf16:
**	vld3.16	{d0\[2\], d2\[2\], d4\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x8x3_t
test_vld3q_lane_bf16 (const bfloat16_t *a, bfloat16x8x3_t b)
{
  return vld3q_lane_bf16 (a, b, 2);
}

/*
**test_vld4_lane_bf16:
**	vld4.16	{d0\[2\], d1\[2\], d2\[2\], d3\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x4x4_t
test_vld4_lane_bf16 (const bfloat16_t *a, bfloat16x4x4_t b)
{
  return vld4_lane_bf16 (a, b, 2);
}

/*
**test_vld4q_lane_bf16:
**	vld4.16	{d0\[2\], d2\[2\], d4\[2\], d6\[2\]}, \[r0\]
**	bx	lr
*/
bfloat16x8x4_t
test_vld4q_lane_bf16 (const bfloat16_t *a, bfloat16x8x4_t b)
{
  return vld4q_lane_bf16 (a, b, 2);
}
