/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vst1_bf16:
**	vst1.16	{d0}, \[r0\]
**	bx	lr
*/
void
test_vst1_bf16 (bfloat16_t *a, bfloat16x4_t b)
{
  vst1_bf16 (a, b);
}

/*
**test_vst1q_bf16:
**	vst1.16	{d0-d1}, \[r0\]
**	bx	lr
*/
void
test_vst1q_bf16 (bfloat16_t *a, bfloat16x8_t b)
{
  vst1q_bf16 (a, b);
}
