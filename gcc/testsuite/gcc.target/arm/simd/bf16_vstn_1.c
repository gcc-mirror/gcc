/* { dg-do assemble } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
**test_vst2_bf16:
**	...
**	vst2.16	{d0-d1}, \[r0\]
**	bx	lr
*/
void
test_vst2_bf16 (bfloat16_t *ptr, bfloat16x4x2_t val)
{
  vst2_bf16 (ptr, val);
}

/*
**test_vst2q_bf16:
**      ...
**	vst2.16	{d0-d3}, \[r0\]
**	bx	lr
*/
void
test_vst2q_bf16 (bfloat16_t *ptr, bfloat16x8x2_t val)
{
  vst2q_bf16 (ptr, val);
}

/*
**test_vst3_bf16:
**      ...
**	vst3.16	{d0-d2}, \[r0\]
**	bx	lr
*/
void
test_vst3_bf16 (bfloat16_t *ptr, bfloat16x4x3_t val)
{
  vst3_bf16 (ptr, val);
}

/*
**test_vst3q_bf16:
**      ...
**	vst3.16	{d17, d19, d21}, \[r0\]
**	bx	lr
*/
void
test_vst3q_bf16 (bfloat16_t *ptr, bfloat16x8x3_t val)
{
  vst3q_bf16 (ptr, val);
}

/*
**test_vst4_bf16:
**      ...
**	vst4.16	{d0-d3}, \[r0\]
**	bx	lr
*/
void
test_vst4_bf16 (bfloat16_t *ptr, bfloat16x4x4_t val)
{
  vst4_bf16 (ptr, val);
}

/*
**test_vst4q_bf16:
**      ...
**	vst4.16	{d1, d3, d5, d7}, \[r0\]
**	bx	lr
*/
void
test_vst4q_bf16 (bfloat16_t *ptr, bfloat16x8x4_t val)
{
  vst4q_bf16 (ptr, val);
}

int main()
{
  return 0;
}
