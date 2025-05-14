/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sve2+sme2")

void
f1 (svbool_t pg, svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
    svint16x2_t s16x2, svuint16x2_t u16x2,
    svint32x2_t s32x2, svuint32x2_t u32x2,
    svint16x3_t s16x3, svuint16x3_t u16x3,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svmla_lane_za32_vg2x1 (0, s16, s16); /* { dg-error {too few arguments to function 'svmla_lane_za32_vg2x1'} } */
  svmla_lane_za32_vg2x1 (0, s16, s16, 0, 0); /* { dg-error {too many arguments to function 'svmla_lane_za32_vg2x1'} } */

  svmla_lane_za32_vg2x1 (s16, s16, s16, 0); /* { dg-error {passing 'svint16_t' to argument 1 of 'svmla_lane_za32_vg2x1', which expects 'uint32_t'} } */
  svmla_lane_za32_vg2x1 (f, s16, s16, 0);
  svmla_lane_za32_vg2x1 (d, s16, s16, 0);
  svmla_lane_za32_vg2x1 (pg, s16, s16, 0); /* { dg-error {passing 'svbool_t' to argument 1 of 'svmla_lane_za32_vg2x1', which expects 'uint32_t'} } */

  svmla_lane_za32_vg2x1 (0, 1, s16, 0); /* { dg-error {passing 'int' to argument 2 of 'svmla_lane_za32_vg2x1', which expects an SVE type rather than a scalar type} } */
  svmla_lane_za32_vg2x1 (0, pg, s16, 0); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmla_lane_za32_vg2x1', but argument 2 had type 'svbool_t'} } */
  svmla_lane_za32_vg2x1 (0, s16x2, s16, 0); /* { dg-error {passing 'svint16x2_t' to argument 2 of 'svmla_lane_za32_vg2x1', which expects a single SVE vector rather than a tuple} } */
  svmla_lane_za32_vg2x1 (0, s16x3, s16, 0); /* { dg-error {passing 'svint16x3_t' to argument 2 of 'svmla_lane_za32_vg2x1', which expects a single SVE vector rather than a tuple} } */

  svmla_lane_za32_vg2x1 (0, s16, 1, 0); /* { dg-error {passing 'int' to argument 3 of 'svmla_lane_za32_vg2x1', which expects an SVE type rather than a scalar type} } */
  svmla_lane_za32_vg2x1 (0, s16, pg, 0); /* { dg-error {passing 'svbool_t' to argument 3 of 'svmla_lane_za32_vg2x1', but argument 2 had type 'svint16_t'} } */
  svmla_lane_za32_vg2x1 (0, s16, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svmla_lane_za32_vg2x1', but argument 2 had type 'svint16_t'} } */
  svmla_lane_za32_vg2x1 (0, s16, s32, 0); /* { dg-error {passing 'svint32_t' to argument 3 of 'svmla_lane_za32_vg2x1', but argument 2 had type 'svint16_t'} } */
  svmla_lane_za32_vg2x1 (0, s16, s16x2, 0); /* { dg-error {passing 'svint16x2_t' to argument 3 of 'svmla_lane_za32_vg2x1', which expects a single SVE vector rather than a tuple} } */
  svmla_lane_za32_vg2x1 (0, u16, u16, 0);
  svmla_lane_za32_vg2x1 (0, u16, s16, 0); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmla_lane_za32_vg2x1', but argument 2 had type 'svuint16_t'} } */
  svmla_lane_za32_vg2x1 (0, s32, s32, 0); /* { dg-error {'svmla_lane_za32_vg2x1' has no form that takes 'svint32_t' arguments} } */
  svmla_lane_za32_vg2x1 (0, u32, u32, 0); /* { dg-error {'svmla_lane_za32_vg2x1' has no form that takes 'svuint32_t' arguments} } */

  svmla_lane_za32_vg2x1 (0, s16, s16, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane_za32_vg2x1', which expects a value in the range \[0, 7\]} } */
  svmla_lane_za32_vg2x1 (0, s16, s16, 7);
  svmla_lane_za32_vg2x1 (0, s16, s16, 8); /* { dg-error {passing 8 to argument 4 of 'svmla_lane_za32_vg2x1', which expects a value in the range \[0, 7\]} } */
  svmla_lane_za32_vg2x1 (0, s16, s16, f); /* { dg-error {argument 4 of 'svmla_lane_za32_vg2x1' must be an integer constant expression} } */
}

void
f2 (svint16x2_t s16x2, svint16_t s16) __arm_streaming
{
  svmla_lane_za32_vg2x1 (0, s16, s16, 0); /* { dg-error {ACLE function 'svmla_lane_za32_s16_vg2x1' can only be called from a function that has 'za' state} } */
}

void
f3 (svint16x2_t s16x2, svint16_t s16) __arm_inout("za")
{
  svmla_lane_za32_vg2x1 (0, s16, s16, 0); /* { dg-error {ACLE function 'svmla_lane_za32_s16_vg2x1' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("+sme-i16i64")

void
f4 (svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64)
  __arm_streaming __arm_inout("za")
{
  svmla_lane_za64_vg4x1 (0, s16, s16, 0);
  svmla_lane_za64_vg4x1 (0, u16, u16, 0);
  svmla_lane_za64_vg4x1 (0, s16, s16, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane_za64_vg4x1', which expects a value in the range \[0, 7\]} } */
  svmla_lane_za64_vg4x1 (0, s16, s16, 7);
  svmla_lane_za64_vg4x1 (0, u16, u16, 8); /* { dg-error {passing 8 to argument 4 of 'svmla_lane_za64_vg4x1', which expects a value in the range \[0, 7\]} } */
  svmla_lane_za64_vg4x1 (0, s32, s32, 0); /* { dg-error {'svmla_lane_za64_vg4x1' has no form that takes 'svint32_t' arguments} } */
  svmla_lane_za64_vg4x1 (0, u32, u32, 0); /* { dg-error {'svmla_lane_za64_vg4x1' has no form that takes 'svuint32_t' arguments} } */
  svmla_lane_za64_vg4x1 (0, s64, s64, 0); /* { dg-error {'svmla_lane_za64_vg4x1' has no form that takes 'svint64_t' arguments} } */
  svmla_lane_za64_vg4x1 (0, u64, u64, 0); /* { dg-error {'svmla_lane_za64_vg4x1' has no form that takes 'svuint64_t' arguments} } */
}
