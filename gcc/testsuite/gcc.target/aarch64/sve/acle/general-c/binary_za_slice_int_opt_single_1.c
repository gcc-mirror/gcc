/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sme2")

void
f1 (svbool_t pg, svint16_t s16, svint8_t s8, svuint8_t u8,
    svint16x2_t s16x2, svuint16x2_t u16x2, svint8x2_t s8x2, svuint8x2_t u8x2,
    svint8x3_t s8x3, svuint8x3_t u8x3,
    svint8x4_t s8x4, svuint8x4_t u8x4,
    svint64x2_t s64x2, svuint64x2_t u64x2,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svusdot_za32_vg1x2 (1, u8x2); /* { dg-error {too few arguments to function 'svusdot_za32_vg1x2'} } */
  svusdot_za32_vg1x2 (1, u8x2, s8x2, s8x2); /* { dg-error {too many arguments to function 'svusdot_za32_vg1x2'} } */

  svusdot_za32_vg1x2 (s8x2, u8x2, s8x2); /* { dg-error {passing 'svint8x2_t' to argument 1 of 'svusdot_za32_vg1x2', which expects 'uint32_t'} } */
  svusdot_za32_vg1x2 (f, u8x2, s8x2);
  svusdot_za32_vg1x2 (d, u8x2, s8x2);
  svusdot_za32_vg1x2 (pg, u8x2, s8x2); /* { dg-error {passing 'svbool_t' to argument 1 of 'svusdot_za32_vg1x2', which expects 'uint32_t'} } */

  svusdot_za32_vg1x2 (1, 1, s8x2); /* { dg-error {passing 'int' to argument 2 of 'svusdot_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svusdot_za32_vg1x2 (1, pg, s8x2); /* { dg-error {passing 'svbool_t' to argument 2 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, s8, s8x2); /* { dg-error {passing single vector 'svint8_t' to argument 2 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, u8x3, s8x3); /* { dg-error {passing 'svuint8x3_t' to argument 2 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, u8x4, s8x4); /* { dg-error {passing 'svuint8x4_t' to argument 2 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */

  svusdot_za32_vg1x2 (1, u8x2, 1); /* { dg-error {passing 'int' to argument 3 of 'svusdot_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svusdot_za32_vg1x2 (1, u8x2, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a vector of signed integers} } */
  svusdot_za32_vg1x2 (1, u8x2, s16); /* { dg-error {arguments 2 and 3 of 'svusdot_za32_vg1x2' must have the same element size, but the values passed here have type 'svuint8x2_t' and 'svint16_t' respectively} } */
  svusdot_za32_vg1x2 (1, u8x2, s16x2); /* { dg-error {arguments 2 and 3 of 'svusdot_za32_vg1x2' must have the same element size, but the values passed here have type 'svuint8x2_t' and 'svint16x2_t' respectively} } */
  svusdot_za32_vg1x2 (1, u8x2, s8);
  svusdot_za32_vg1x2 (1, u8x2, s8x2);
  svusdot_za32_vg1x2 (1, u8x2, s8x3); /* { dg-error {passing 'svint8x3_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, u8x2, s8x4); /* { dg-error {passing 'svint8x4_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, u8x2, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a vector of signed integers} } */
  svusdot_za32_vg1x2 (1, u8x2, u8x2); /* { dg-error {passing 'svuint8x2_t' to argument 3 of 'svusdot_za32_vg1x2', which expects vectors of signed integers} } */
  svusdot_za32_vg1x2 (1, u8x2, s8x3); /* { dg-error {passing 'svint8x3_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, u8x2, s8x4); /* { dg-error {passing 'svint8x4_t' to argument 3 of 'svusdot_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svusdot_za32_vg1x2 (1, s8x2, s8); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svint8x2_t' arguments} } */
  svusdot_za32_vg1x2 (1, s8x2, s8x2); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svint8x2_t' arguments} } */

  svusdot_za32_vg1x2 (1, u16x2, s16); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svuint16x2_t' arguments} } */
  svusdot_za32_vg1x2 (1, u16x2, s16x2); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svuint16x2_t' arguments} } */
  svusdot_za32_vg1x2 (1, s64x2, s64x2); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svint64x2_t' arguments} } */
  svusdot_za32_vg1x2 (1, u64x2, s64x2); /* { dg-error {'svusdot_za32_vg1x2' has no form that takes 'svuint64x2_t' arguments} } */
}

void
f2 (svint8x2_t s8x2, svuint8x2_t u8x2) __arm_streaming
{
  svusdot_za32_vg1x2 (0, u8x2, s8x2); /* { dg-error {ACLE function 'svusdot_za32_u8_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svint8x2_t s8x2, svuint8x2_t u8x2) __arm_inout("za")
{
  svusdot_za32_vg1x2 (0, u8x2, s8x2); /* { dg-error {ACLE function 'svusdot_za32_u8_vg1x2' can only be called when SME streaming mode is enabled} } */
}
