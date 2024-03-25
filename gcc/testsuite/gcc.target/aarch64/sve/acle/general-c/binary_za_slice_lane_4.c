/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sme2")

void
f1 (svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint32x2_t s32x2, svuint32x2_t u32x2, svfloat32x2_t f32x2, int i)
  __arm_streaming __arm_inout("za")
{
  svmla_lane_za32_vg4x1 (0, s8, s8, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane_za32_vg4x1', which expects a value in the range \[0, 15\]} } */
  svmla_lane_za32_vg4x1 (0, u8, u8, 0);
  svmla_lane_za32_vg4x1 (0, s8, s8, 15);
  svmla_lane_za32_vg4x1 (0, u8, u8, 16); /* { dg-error {passing 16 to argument 4 of 'svmla_lane_za32_vg4x1', which expects a value in the range \[0, 15\]} } */
  svmla_lane_za32_vg4x1 (0, s16, s16, 0); /* { dg-error {'svmla_lane_za32_vg4x1' has no form that takes 'svint16_t' arguments} } */
  svmla_lane_za32_vg4x1 (0, u16, u16, 0); /* { dg-error {'svmla_lane_za32_vg4x1' has no form that takes 'svuint16_t' arguments} } */

  svmla_lane_za32_vg1x2 (0, s32x2, s32, 0); /* { dg-error {'svmla_lane_za32_vg1x2' has no form that takes 'svint32x2_t' arguments} } */
  svmla_lane_za32_vg1x2 (0, u32x2, u32, 0); /* { dg-error {'svmla_lane_za32_vg1x2' has no form that takes 'svuint32x2_t' arguments} } */
  svmla_lane_za32_vg1x2 (0, f32x2, f32, 0);
  svmla_lane_za32_vg1x2 (0, f32x2, f32, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svmla_lane_za32_vg1x2 (0, f32x2, f32, 4); /* { dg-error {passing 4 to argument 4 of 'svmla_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svmla_lane_za32_vg1x2 (0, f32x2, f32, i); /* { dg-error {argument 4 of 'svmla_lane_za32_vg1x2' must be an integer constant expression} } */
}
