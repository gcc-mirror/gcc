/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sve2+sme2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint8x2_t s8x2, svuint8x2_t u8x2,
    svint8x3_t s8x3, svuint8x3_t u8x3,
    svint8x4_t s8x4, svuint8x4_t u8x4,
    svint16x2_t s16x2, svuint16x2_t u16x2,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svusdot_lane_za32_vg1x2 (0, u8x2, s8); /* { dg-error {too few arguments to function 'svusdot_lane_za32_vg1x2'} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 0, 0); /* { dg-error {too many arguments to function 'svusdot_lane_za32_vg1x2'} } */

  svusdot_lane_za32_vg1x2 (u8x2, u8x2, s8, 0); /* { dg-error {passing 'svuint8x2_t' to argument 1 of 'svusdot_lane_za32_vg1x2', which expects 'uint32_t'} } */
  svusdot_lane_za32_vg1x2 (f, u8x2, s8, 0);
  svusdot_lane_za32_vg1x2 (d, u8x2, s8, 0);
  svusdot_lane_za32_vg1x2 (pg, u8x2, s8, 0); /* { dg-error {passing 'svbool_t' to argument 1 of 'svusdot_lane_za32_vg1x2', which expects 'uint32_t'} } */

  svusdot_lane_za32_vg1x2 (0, 1, s8, 0); /* { dg-error {passing 'int' to argument 2 of 'svusdot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svusdot_lane_za32_vg1x2 (0, pg, s8, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svusdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_lane_za32_vg1x2 (0, u8, s8, 0); /* { dg-error {passing single vector 'svuint8_t' to argument 2 of 'svusdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_lane_za32_vg1x2 (0, u8x3, s8, 0); /* { dg-error {passing 'svuint8x3_t' to argument 2 of 'svusdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_lane_za32_vg1x2 (0, u8x4, s8, 0); /* { dg-error {passing 'svuint8x4_t' to argument 2 of 'svusdot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */

  svusdot_lane_za32_vg1x2 (0, u8x2, 1, 0); /* { dg-error {passing 'int' to argument 3 of 'svusdot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, pg, 0); /* { dg-error {passing 'svbool_t' to argument 3 of 'svusdot_lane_za32_vg1x2', which expects a vector of signed integers} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, u8, 0); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusdot_lane_za32_vg1x2', which expects a vector of signed integers} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s32, 0); /* { dg-error {arguments 2 and 3 of 'svusdot_lane_za32_vg1x2' must have the same element size, but the values passed here have type 'svuint8x2_t' and 'svint32_t' respectively} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8x2, 0); /* { dg-error {passing 'svint8x2_t' to argument 3 of 'svusdot_lane_za32_vg1x2', which expects a single SVE vector rather than a tuple} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 0);
  svusdot_lane_za32_vg1x2 (0, s8x2, s8, 0); /* { dg-error {'svusdot_lane_za32_vg1x2' has no form that takes 'svint8x2_t' arguments} } */
  svusdot_lane_za32_vg1x2 (0, u16x2, s16, 0); /* { dg-error {'svusdot_lane_za32_vg1x2' has no form that takes 'svuint16x2_t' arguments} } */

  svusdot_lane_za32_vg1x2 (0, u8x2, s8, -1); /* { dg-error {passing -1 to argument 4 of 'svusdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 3);
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 4); /* { dg-error {passing 4 to argument 4 of 'svusdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, -1); /* { dg-error {passing -1 to argument 4 of 'svusdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 3);
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 4); /* { dg-error {passing 4 to argument 4 of 'svusdot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, f); /* { dg-error {argument 4 of 'svusdot_lane_za32_vg1x2' must be an integer constant expression} } */
}

void
f2 (svuint8x2_t u8x2, svint8_t s8) __arm_streaming
{
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 0); /* { dg-error {ACLE function 'svusdot_lane_za32_u8_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svuint8x2_t u8x2, svint8_t s8) __arm_inout("za")
{
  svusdot_lane_za32_vg1x2 (0, u8x2, s8, 0); /* { dg-error {ACLE function 'svusdot_lane_za32_u8_vg1x2' can only be called when SME streaming mode is enabled} } */
}
