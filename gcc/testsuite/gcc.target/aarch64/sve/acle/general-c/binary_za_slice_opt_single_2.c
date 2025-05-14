/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sve2+sme2")

void
f1 (svbool_t pg, svint16_t s16, svint32_t s32, svuint32_t u32,
    svint16x2_t s16x2, svint32x2_t s32x2, svuint32x2_t u32x2,
    svint32x3_t s32x3, svuint32x3_t u32x3,
    svint32x4_t s32x4, svuint32x4_t u32x4,
    svint64x2_t s64x2, svuint64x2_t u64x2,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svadd_write_za32_vg1x4 (1, s32x4); /* { dg-error {too few arguments to function 'svadd_write_za32_vg1x4'} } */
  svadd_write_za32_vg1x4 (1, s32x4, s32x4, s32x4); /* { dg-error {too many arguments to function 'svadd_write_za32_vg1x4'} } */

  svadd_write_za32_vg1x4 (s32x4, s32x4, s32x4); /* { dg-error {passing 'svint32x4_t' to argument 1 of 'svadd_write_za32_vg1x4', which expects 'uint32_t'} } */
  svadd_write_za32_vg1x4 (f, s32x4, s32x4);
  svadd_write_za32_vg1x4 (d, s32x4, s32x4);
  svadd_write_za32_vg1x4 (pg, s32x4, s32x4); /* { dg-error {passing 'svbool_t' to argument 1 of 'svadd_write_za32_vg1x4', which expects 'uint32_t'} } */

  svadd_write_za32_vg1x4 (1, 1, s32x4); /* { dg-error {passing 'int' to argument 2 of 'svadd_write_za32_vg1x4', which expects an SVE type rather than a scalar} } */
  svadd_write_za32_vg1x4 (1, pg, s32x4); /* { dg-error {passing 'svbool_t' to argument 2 of 'svadd_write_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_write_za32_vg1x4 (1, s32, s32x4); /* { dg-error {passing single vector 'svint32_t' to argument 2 of 'svadd_write_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_write_za32_vg1x4 (1, s32x2, s32x2); /* { dg-error {passing 'svint32x2_t' to argument 2 of 'svadd_write_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_write_za32_vg1x4 (1, s32x3, s32x3); /* { dg-error {passing 'svint32x3_t' to argument 2 of 'svadd_write_za32_vg1x4', which expects a tuple of 4 vectors} } */
}
