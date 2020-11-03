/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vld1_bf16:
**	vld1.16	{d0}, \[r0\]
**	bx	lr
*/
bfloat16x4_t
test_vld1_bf16 (bfloat16_t const *p)
{
  return vld1_bf16 (p);
}

/*
**test_vld1q_bf16:
**	vld1.16	{d0-d1}, \[r0\]
**	bx	lr
*/
bfloat16x8_t
test_vld1q_bf16 (bfloat16_t const *p)
{
  return vld1q_bf16 (p);
}
