/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>

/*
**test_vmmlaq_f32:
**        ...
**        vmmla.bf16	q0, q1, q2
**        bx	lr
*/
float32x4_t
test_vmmlaq_f32 (float32x4_t r, bfloat16x8_t x, bfloat16x8_t y)
{
  return vbfmmlaq_f32 (r, x, y);
}
