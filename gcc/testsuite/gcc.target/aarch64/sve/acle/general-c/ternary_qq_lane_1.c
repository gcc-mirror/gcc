/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat32_t f32, int i)
{
  svdot_lane (u32, u8, u8); /* { dg-error {too few arguments to function 'svdot_lane'} } */
  svdot_lane (u32, u8, u8, 0, 0); /* { dg-error {too many arguments to function 'svdot_lane'} } */
  svdot_lane (0, u8, u8, 0); /* { dg-error {passing 'int' to argument 1 of 'svdot_lane', which expects an SVE type rather than a scalar} } */
  svdot_lane (pg, u8, u8, 0); /* { dg-error {'svdot_lane' has no form that takes 'svbool_t' and 'svuint8_t' arguments} } */
  svdot_lane (u8, u8, u8, 0); /* { dg-error {'svdot_lane' has no form that takes 'svuint8_t' and 'svuint8_t' arguments} } */
  svdot_lane (f32, u8, u8, 0); /* { dg-error {'svdot_lane' has no form that takes 'svfloat32_t' and 'svuint8_t' arguments} } */
  svdot_lane (u32, u8, u8, 0);
  svdot_lane (u32, 0, u8, 0); /* { dg-error {passing 'int' to argument 2 of 'svdot_lane', which expects an SVE type rather than a scalar} } */
  svdot_lane (u32, u8, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svdot_lane', which expects an SVE type rather than a scalar} } */

  svdot_lane (s32, s8, s8, 0);
  svdot_lane (s32, u8, s8, 0); /* { dg-error {passing 'svint8_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint8_t'} } */
  svdot_lane (s32, s8, u8, 0); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svint8_t'} } */
  svdot_lane (s32, s32, s32, 0); /* { dg-error {'svdot_lane' has no form that takes 'svint32_t' and 'svint32_t' arguments} } */

  svdot_lane (u32, u8, u8, 0);
  svdot_lane (u32, s8, u8, 0); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svint8_t'} } */
  svdot_lane (u32, u8, s8, 0); /* { dg-error {passing 'svint8_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint8_t'} } */
  svdot_lane (u32, u32, u32, 0); /* { dg-error {'svdot_lane' has no form that takes 'svuint32_t' and 'svuint32_t' arguments} } */

  svdot_lane (s64, s16, s16, 0);
  svdot_lane (s64, u16, s16, 0); /* { dg-error {passing 'svint16_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint16_t'} } */
  svdot_lane (s64, s16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svint16_t'} } */
  svdot_lane (s64, s64, s64, 0); /* { dg-error {'svdot_lane' has no form that takes 'svint64_t' and 'svint64_t' arguments} } */

  svdot_lane (u64, u16, u16, 0);
  svdot_lane (u64, s16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svint16_t'} } */
  svdot_lane (u64, u16, s16, 0); /* { dg-error {passing 'svint16_t' to argument 3 of 'svdot_lane', but argument 2 had type 'svuint16_t'} } */
  svdot_lane (u64, u64, u64, 0); /* { dg-error {'svdot_lane' has no form that takes 'svuint64_t' and 'svuint64_t' arguments} } */

  svdot_lane (s32, s8, s8, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (s32, s8, s8, 0);
  svdot_lane (s32, s8, s8, 3);
  svdot_lane (s32, s8, s8, 4);  /* { dg-error {passing 4 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
  svdot_lane (s32, s8, s8, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */

  svdot_lane (u32, u8, u8, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (u32, u8, u8, 0);
  svdot_lane (u32, u8, u8, 3);
  svdot_lane (u32, u8, u8, 4);  /* { dg-error {passing 4 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */
  svdot_lane (u32, u8, u8, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 3\]} } */

  svdot_lane (s64, s16, s16, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (s64, s16, s16, 0);
  svdot_lane (s64, s16, s16, 1);
  svdot_lane (s64, s16, s16, 2);  /* { dg-error {passing 2 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 1\]} } */
  svdot_lane (s64, s16, s16, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 1\]} } */

  svdot_lane (u64, u16, u16, i); /* { dg-error {argument 4 of 'svdot_lane' must be an integer constant expression} } */
  svdot_lane (u64, u16, u16, 0);
  svdot_lane (u64, u16, u16, 1);
  svdot_lane (u64, u16, u16, 2);  /* { dg-error {passing 2 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 1\]} } */
  svdot_lane (u64, u16, u16, -1);  /* { dg-error {passing -1 to argument 4 of 'svdot_lane', which expects a value in the range \[0, 1\]} } */
}
