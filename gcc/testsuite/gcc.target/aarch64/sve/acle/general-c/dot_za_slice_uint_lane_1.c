/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sme2")

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
  svsudot_lane_za32_vg1x2 (0, s8x2, u8); /* { dg-error {too few arguments to function 'svsudot_lane_za32_vg1x2'} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 0, 0); /* { dg-error {too many arguments to function 'svsudot_lane_za32_vg1x2'} } */

  svsudot_lane_za32_vg1x2 (u8x2, s8x2, u8, 0); /* { dg-error {passing 'svuint8x2_t' to argument 1 of 'svsudot_lane_za32_vg1x2', which expects 'uint32_t'} } */
  svsudot_lane_za32_vg1x2 (f, s8x2, u8, 0);
  svsudot_lane_za32_vg1x2 (d, s8x2, u8, 0);
  svsudot_lane_za32_vg1x2 (pg, s8x2, u8, 0); /* { dg-error {passing 'svbool_t' to argument 1 of 'svsudot_lane_za32_vg1x2', which expects 'uint32_t'} } */

  svsudot_lane_za32_vg1x2 (0, 1, u8, 0); /* { dg-error {passing 'int' to argument 2 of 'svsudot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svsudot_lane_za32_vg1x2 (0, pg, u8, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svsudot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svsudot_lane_za32_vg1x2 (0, s8, u8, 0); /* { dg-error {passing single vector 'svint8_t' to argument 2 of 'svsudot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svsudot_lane_za32_vg1x2 (0, s8x3, u8, 0); /* { dg-error {passing 'svint8x3_t' to argument 2 of 'svsudot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svsudot_lane_za32_vg1x2 (0, s8x4, u8, 0); /* { dg-error {passing 'svint8x4_t' to argument 2 of 'svsudot_lane_za32_vg1x2', which expects a tuple of 2 vectors} } */

  svsudot_lane_za32_vg1x2 (0, s8x2, 1, 0); /* { dg-error {passing 'int' to argument 3 of 'svsudot_lane_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, pg, 0); /* { dg-error {passing 'svbool_t' to argument 3 of 'svsudot_lane_za32_vg1x2', which expects a vector of unsigned integers} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, s8, 0); /* { dg-error {passing 'svint8_t' to argument 3 of 'svsudot_lane_za32_vg1x2', which expects a vector of unsigned integers} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u32, 0); /* { dg-error {arguments 2 and 3 of 'svsudot_lane_za32_vg1x2' must have the same element size, but the values passed here have type 'svint8x2_t' and 'svuint32_t' respectively} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8x2, 0); /* { dg-error {passing 'svuint8x2_t' to argument 3 of 'svsudot_lane_za32_vg1x2', which expects a single SVE vector rather than a tuple} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 0);
  svsudot_lane_za32_vg1x2 (0, u8x2, u8, 0); /* { dg-error {'svsudot_lane_za32_vg1x2' has no form that takes 'svuint8x2_t' arguments} } */
  svsudot_lane_za32_vg1x2 (0, s16x2, u16, 0); /* { dg-error {'svsudot_lane_za32_vg1x2' has no form that takes 'svint16x2_t' arguments} } */

  svsudot_lane_za32_vg1x2 (0, s8x2, u8, -1); /* { dg-error {passing -1 to argument 4 of 'svsudot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 3);
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 4); /* { dg-error {passing 4 to argument 4 of 'svsudot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, -1); /* { dg-error {passing -1 to argument 4 of 'svsudot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 3);
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 4); /* { dg-error {passing 4 to argument 4 of 'svsudot_lane_za32_vg1x2', which expects a value in the range \[0, 3\]} } */
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, f); /* { dg-error {argument 4 of 'svsudot_lane_za32_vg1x2' must be an integer constant expression} } */
}

void
f2 (svint8x2_t s8x2, svuint8_t u8) __arm_streaming
{
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 0); /* { dg-error {ACLE function 'svsudot_lane_za32_s8_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svint8x2_t s8x2, svuint8_t u8) __arm_inout("za")
{
  svsudot_lane_za32_vg1x2 (0, s8x2, u8, 0); /* { dg-error {ACLE function 'svsudot_lane_za32_s8_vg1x2' can only be called when SME streaming mode is enabled} } */
}
