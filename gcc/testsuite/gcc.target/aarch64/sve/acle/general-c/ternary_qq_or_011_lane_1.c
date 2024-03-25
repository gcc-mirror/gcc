/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sme2"

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32, int i) __arm_streaming
{
  svdot_lane (u32, u16, u8, 0); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint16_t'} } */
  svdot_lane (u32, u8, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint8_t'} } */
  svdot_lane (u32, s16, s16, 0); /* { dg-error {'svdot_lane' has no form that takes 'svuint32_t' and 'svint16_t' arguments} } */

  svdot_lane (u32, u16, u16, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (u32, u16, u16, 0);
  svdot_lane (u32, u16, u16, 3);
  svdot_lane (u32, u16, u16, 4);  /* { dg-error {passing 4 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
  svdot_lane (u32, u16, u16, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */

  svdot_lane (s32, s16, s16, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (s32, s16, s16, 0);
  svdot_lane (s32, s16, s16, 3);
  svdot_lane (s32, s16, s16, 4);  /* { dg-error {passing 4 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
  svdot_lane (s32, s16, s16, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */

  svdot_lane (f32, f16, f16, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (f32, f16, f16, 0);
  svdot_lane (f32, f16, f16, 3);
  svdot_lane (f32, f16, f16, 4);  /* { dg-error {passing 4 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
  svdot_lane (f32, f16, f16, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
}
