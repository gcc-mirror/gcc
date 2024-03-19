/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme2")

void
f1 (svbool_t pg, svint32_t s32, svint16x4_t s16x4, svint32x2_t s32x2,
    svint32x3_t s32x3, svint32x4_t s32x4, svint64x4_t s64x4, float f, double d)
  __arm_streaming __arm_inout("za")
{
  svadd_za32_vg1x4 (1); /* { dg-error {too few arguments to function 'svadd_za32_vg1x4'} } */
  svadd_za32_vg1x4 (1, s32x4, s32x4); /* { dg-error {too many arguments to function 'svadd_za32_vg1x4'} } */

  svadd_za32_vg1x4 (s32x2, s32x4); /* { dg-error {passing 'svint32x2_t' to argument 1 of 'svadd_za32_vg1x4', which expects 'uint32_t'} } */
  svadd_za32_vg1x4 (f, s32x4);
  svadd_za32_vg1x4 (d, s32x4);
  svadd_za32_vg1x4 (pg, s32x4); /* { dg-error {passing 'svbool_t' to argument 1 of 'svadd_za32_vg1x4', which expects 'uint32_t'} } */

  svadd_za32_vg1x4 (1, s32); /* { dg-error {passing single vector 'svint32_t' to argument 2 of 'svadd_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_za32_vg1x4 (1, s32x2); /* { dg-error {passing 'svint32x2_t' to argument 2 of 'svadd_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_za32_vg1x4 (1, s32x3); /* { dg-error {passing 'svint32x3_t' to argument 2 of 'svadd_za32_vg1x4', which expects a tuple of 4 vectors} } */
  svadd_za32_vg1x4 (1, s32x4);

  svadd_za32_vg1x4 (1, s16x4); /* { dg-error {'svadd_za32_vg1x4' has no form that takes 'svint16x4_t' arguments} } */
  svadd_za32_vg1x4 (1, s64x4); /* { dg-error {'svadd_za32_vg1x4' has no form that takes 'svint64x4_t' arguments} } */
}
