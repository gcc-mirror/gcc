/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target "+sve2+sme2"

void
f1 (svint8_t s8, svint8x2_t s8x2, svint8x3_t s8x3, svint8x4_t s8x4,
    svuint8_t u8, svuint16x2_t u16x2, svfloat32x2_t f32x2, svint64x2_t s64x2,
    uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svwrite_za8_vg1x2 (0); /* { dg-error {too few arguments to function 'svwrite_za8_vg1x2'} } */
  svwrite_za8_vg1x2 (0, s8x2, 0); /* { dg-error {too many arguments to function 'svwrite_za8_vg1x2'} } */
  svwrite_za8_vg1x2 (u8, s8x2); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svwrite_za8_vg1x2', which expects 'uint32_t'} } */
  svwrite_za8_vg1x2 (0, tile); /* { dg-error {passing 'uint32_t'.* to argument 2 of 'svwrite_za8_vg1x2', which expects an SVE type} } */
  svwrite_za8_vg1x2 (0, s8); /* { dg-error {passing single vector 'svint8_t' to argument 2 of 'svwrite_za8_vg1x2', which expects a tuple of 2 vectors} } */
  svwrite_za8_vg1x2 (0, s8x2);
  svwrite_za8_vg1x2 (0, s8x3); /* { dg-error {passing 'svint8x3_t' to argument 2 of 'svwrite_za8_vg1x2', which expects a tuple of 2 vectors} } */
  svwrite_za8_vg1x2 (0, s8x4); /* { dg-error {passing 'svint8x4_t' to argument 2 of 'svwrite_za8_vg1x2', which expects a tuple of 2 vectors} } */

  svwrite_za8_vg1x4 (0, s8); /* { dg-error {passing single vector 'svint8_t' to argument 2 of 'svwrite_za8_vg1x4', which expects a tuple of 4 vectors} } */
  svwrite_za8_vg1x4 (0, s8x2); /* { dg-error {passing 'svint8x2_t' to argument 2 of 'svwrite_za8_vg1x4', which expects a tuple of 4 vectors} } */
  svwrite_za8_vg1x4 (0, s8x3); /* { dg-error {passing 'svint8x3_t' to argument 2 of 'svwrite_za8_vg1x4', which expects a tuple of 4 vectors} } */
  svwrite_za8_vg1x4 (0, s8x4);
}

void
f2 (svint8x2_t s8x2) __arm_streaming
{
  svwrite_za8_vg1x2 (0, s8x2); /* { dg-error {ACLE function 'svwrite_za8_s8_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svint8x2_t s8x2) __arm_inout("za")
{
  svwrite_za8_vg1x2 (0, s8x2); /* { dg-error {ACLE function 'svwrite_za8_s8_vg1x2' can only be called when SME streaming mode is enabled} } */
}
