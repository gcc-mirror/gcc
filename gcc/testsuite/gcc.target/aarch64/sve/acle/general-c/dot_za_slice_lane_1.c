/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sve2+sme2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint8x2_t s8x2, svuint8x2_t u8x2,
    svint16x2_t s16x2, svuint16x2_t u16x2,
    svint16x3_t s16x3, svuint16x3_t u16x3,
    svint16x4_t s16x4, svuint16x4_t u16x4,
    svint32x2_t s32x2, svuint32x2_t u32x2,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svdot_lane_za32_vg1x2 (0, s16x2, s16); /* { dg-error {too few arguments to function 'svdot_lane_za32_vg1x2'} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s16, 0, 0); /* { dg-error {too many arguments to function 'svdot_lane_za32_vg1x2'} } */

  svdot_lane_za32_vg1x2 (s16x2, s16x2, s16, 0); /* { dg-error {passing 'svint16x2_t' to argument 1 of 'svdot_lane_za32_vg1x2', which expects 'uint32_t'} } */
  svdot_lane_za32_vg1x2 (f, s16x2, s16, 0);
  svdot_lane_za32_vg1x2 (d, s16x2, s16, 0);
  svdot_lane_za32_vg1x2 (pg, s16x2, s16, 0); /* { dg-error {passing 'svbool_t' to argument 1 of 'svdot_lane_za32_vg1x2', which expects 'uint32_t'} } */

  svdot_lane_za32_vg1x2 (0, 1, s16, 0); /* { dg-error {passing 'int' to argument 2 of 'svdot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svdot_lane_za32_vg1x2 (0, pg, s16, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svdot_lane_za32_vg1x2 (0, s16, s16, 0); /* { dg-error {passing single vector 'svint16_t' to argument 2 of 'svdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svdot_lane_za32_vg1x2 (0, s16x3, s16, 0); /* { dg-error {passing 'svint16x3_t' to argument 2 of 'svdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svdot_lane_za32_vg1x2 (0, s16x4, s16, 0); /* { dg-error {passing 'svint16x4_t' to argument 2 of 'svdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */

  svdot_lane_za32_vg1x2 (0, s16x2, 1, 0); /* { dg-error {passing 'int' to argument 3 of 'svdot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svdot_lane_za32_vg1x2 (0, s16x2, pg, 0); /* { dg-error {passing 'svbool_t' to argument 3 of 'svdot_lane_za32_vg1x2', but argument 2 was a tuple of 'svint16_t'} } */
  svdot_lane_za32_vg1x2 (0, s16x2, u16, 0); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svdot_lane_za32_vg1x2', but argument 2 was a tuple of 'svint16_t'} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s32, 0); /* { dg-error {passing 'svint32_t' to argument 3 of 'svdot_lane_za32_vg1x2', but argument 2 was a tuple of 'svint16_t'} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s16x2, 0); /* { dg-error {passing 'svint16x2_t' to argument 3 of 'svdot_lane_za32_vg1x2', which expects a single SVE vector rather than a tuple} } */
  svdot_lane_za32_vg1x2 (0, u16x2, u16, 0);
  svdot_lane_za32_vg1x2 (0, u16x2, s16, 0); /* { dg-error {passing 'svint16_t' to argument 3 of 'svdot_lane_za32_vg1x2', but argument 2 was a tuple of 'svuint16_t'} } */
  svdot_lane_za32_vg1x2 (0, s32x2, s32, 0); /* { dg-error {'svdot_lane_za32_vg1x2' has no form that takes 'svint32x2_t' arguments} } */
  svdot_lane_za32_vg1x2 (0, u32x2, u32, 0); /* { dg-error {'svdot_lane_za32_vg1x2' has no form that takes 'svuint32x2_t' arguments} } */

  svdot_lane_za32_vg1x2 (0, s8x2, s8, -1); /* { dg-error {passing -1 to argument 4 of 'svdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svdot_lane_za32_vg1x2 (0, s8x2, s8, 3);
  svdot_lane_za32_vg1x2 (0, s8x2, s8, 4); /* { dg-error {passing 4 to argument 4 of 'svdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s16, -1); /* { dg-error {passing -1 to argument 4 of 'svdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s16, 3);
  svdot_lane_za32_vg1x2 (0, s16x2, s16, 4); /* { dg-error {passing 4 to argument 4 of 'svdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svdot_lane_za32_vg1x2 (0, s16x2, s16, f); /* { dg-error {argument 4 of 'svdot_lane_za32_vg1x2' must be an integer constant expression} } */
}

void
f2 (svint16x2_t s16x2, svint16_t s16) __arm_streaming
{
  svdot_lane_za32_vg1x2 (0, s16x2, s16, 0); /* { dg-error {ACLE function 'svdot_lane_za32_s16_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svint16x2_t s16x2, svint16_t s16) __arm_inout("za")
{
  svdot_lane_za32_vg1x2 (0, s16x2, s16, 0); /* { dg-error {ACLE function 'svdot_lane_za32_s16_vg1x2' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("+sme-i16i64")

void
f4 (svint16_t s16, svuint16_t u16,
    svint16x2_t s16x2, svuint16x2_t u16x2,
    svint32_t s32, svuint32_t u32,
    svint32x2_t s32x2, svuint32x2_t u32x2,
    svint64_t s64, svuint64_t u64,
    svint64x2_t s64x2, svuint64x2_t u64x2)
  __arm_streaming __arm_inout("za")
{
  svdot_lane_za64_vg1x2 (0, s16x2, s16, 0);
  svdot_lane_za64_vg1x2 (0, u16x2, u16, 0);
  svdot_lane_za64_vg1x2 (0, s16x2, s16, -1); /* { dg-error {passing -1 to argument 4 of 'svdot_lane_za64_vg1x2', which expects a value in the range \[0, 1\]} } */
  svdot_lane_za64_vg1x2 (0, s16x2, s16, 1);
  svdot_lane_za64_vg1x2 (0, u16x2, u16, 2); /* { dg-error {passing 2 to argument 4 of 'svdot_lane_za64_vg1x2', which expects a value in the range \[0, 1\]} } */
  svdot_lane_za64_vg1x2 (0, s32x2, s32, 0); /* { dg-error {'svdot_lane_za64_vg1x2' has no form that takes 'svint32x2_t' arguments} } */
  svdot_lane_za64_vg1x2 (0, u32x2, u32, 0); /* { dg-error {'svdot_lane_za64_vg1x2' has no form that takes 'svuint32x2_t' arguments} } */
  svdot_lane_za64_vg1x2 (0, s64x2, s64, 0); /* { dg-error {'svdot_lane_za64_vg1x2' has no form that takes 'svint64x2_t' arguments} } */
  svdot_lane_za64_vg1x2 (0, u64x2, u64, 0); /* { dg-error {'svdot_lane_za64_vg1x2' has no form that takes 'svuint64x2_t' arguments} } */
}
