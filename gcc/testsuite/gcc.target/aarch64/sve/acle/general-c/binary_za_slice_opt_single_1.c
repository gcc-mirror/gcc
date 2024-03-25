/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("+sme2")

void
f1 (svbool_t pg, svint16_t s16, svint32_t s32, svuint32_t u32,
    svint16x2_t s16x2, svint32x2_t s32x2, svuint32x2_t u32x2,
    svint32x3_t s32x3, svuint32x3_t u32x3,
    svint32x4_t s32x4, svuint32x4_t u32x4,
    svint64x2_t s64x2, svuint64x2_t u64x2,
    float f, double d)
  __arm_streaming __arm_inout("za")
{
  svadd_write_za32_vg1x2 (1, s32x2); /* { dg-error {too few arguments to function 'svadd_write_za32_vg1x2'} } */
  svadd_write_za32_vg1x2 (1, s32x2, s32x2, s32x2); /* { dg-error {too many arguments to function 'svadd_write_za32_vg1x2'} } */

  svadd_write_za32_vg1x2 (s32x2, s32x2, s32x2); /* { dg-error {passing 'svint32x2_t' to argument 1 of 'svadd_write_za32_vg1x2', which expects 'uint32_t'} } */
  svadd_write_za32_vg1x2 (f, s32x2, s32x2);
  svadd_write_za32_vg1x2 (d, s32x2, s32x2);
  svadd_write_za32_vg1x2 (pg, s32x2, s32x2); /* { dg-error {passing 'svbool_t' to argument 1 of 'svadd_write_za32_vg1x2', which expects 'uint32_t'} } */

  svadd_write_za32_vg1x2 (1, 1, s32x2); /* { dg-error {passing 'int' to argument 2 of 'svadd_write_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svadd_write_za32_vg1x2 (1, pg, s32x2); /* { dg-error {passing 'svbool_t' to argument 2 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32, s32x2); /* { dg-error {passing single vector 'svint32_t' to argument 2 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32x3, s32x3); /* { dg-error {passing 'svint32x3_t' to argument 2 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32x4, s32x4); /* { dg-error {passing 'svint32x4_t' to argument 2 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */

  svadd_write_za32_vg1x2 (1, s32x2, 1); /* { dg-error {passing 'int' to argument 3 of 'svadd_write_za32_vg1x2', which expects an SVE type rather than a scalar type} } */
  svadd_write_za32_vg1x2 (1, s32x2, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 was a tuple of 'svint32_t'} } */
  svadd_write_za32_vg1x2 (1, s32x2, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 was a tuple of 'svint32_t'} } */
  svadd_write_za32_vg1x2 (1, s32x2, s16x2); /* { dg-error {passing 'svint16x2_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 had type 'svint32x2_t'} } */
  svadd_write_za32_vg1x2 (1, s32x2, s32);
  svadd_write_za32_vg1x2 (1, s32x2, s32x2);
  svadd_write_za32_vg1x2 (1, s32x2, s32x3); /* { dg-error {passing 'svint32x3_t' to argument 3 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32x2, s32x4); /* { dg-error {passing 'svint32x4_t' to argument 3 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32x2, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 was a tuple of 'svint32_t'} } */
  svadd_write_za32_vg1x2 (1, s32x2, u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 had type 'svint32x2_t'} } */
  svadd_write_za32_vg1x2 (1, s32x2, u32x3); /* { dg-error {passing 'svuint32x3_t' to argument 3 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, s32x2, u32x4); /* { dg-error {passing 'svuint32x4_t' to argument 3 of 'svadd_write_za32_vg1x2', which expects a tuple of 2 vectors} } */
  svadd_write_za32_vg1x2 (1, u32x2, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 was a tuple of 'svuint32_t'} } */
  svadd_write_za32_vg1x2 (1, u32x2, s32x2); /* { dg-error {passing 'svint32x2_t' to argument 3 of 'svadd_write_za32_vg1x2', but argument 2 had type 'svuint32x2_t'} } */
  svadd_write_za32_vg1x2 (1, u32x2, u32);
  svadd_write_za32_vg1x2 (1, u32x2, u32x2);

  svadd_write_za32_vg1x2 (1, s16x2, s16); /* { dg-error {'svadd_write_za32_vg1x2' has no form that takes 'svint16x2_t' arguments} } */
  svadd_write_za32_vg1x2 (1, s16x2, s16x2); /* { dg-error {'svadd_write_za32_vg1x2' has no form that takes 'svint16x2_t' arguments} } */
  svadd_write_za32_vg1x2 (1, s64x2, s64x2); /* { dg-error {'svadd_write_za32_vg1x2' has no form that takes 'svint64x2_t' arguments} } */
  svadd_write_za32_vg1x2 (1, u64x2, u64x2); /* { dg-error {'svadd_write_za32_vg1x2' has no form that takes 'svuint64x2_t' arguments} } */
}

void
f2 (svint32x2_t s32x2) __arm_streaming
{
  svadd_write_za32_vg1x2 (0, s32x2, s32x2); /* { dg-error {ACLE function 'svadd_write_za32_s32_vg1x2' can only be called from a function that has 'za' state} } */
}

void
f3 (svint32x2_t s32x2) __arm_inout("za")
{
  svadd_write_za32_vg1x2 (0, s32x2, s32x2); /* { dg-error {ACLE function 'svadd_write_za32_s32_vg1x2' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("+sme-i16i64")

void
f4 (svint32x2_t s32x2, svuint32x2_t u32x2,
    svint64x2_t s64x2, svuint64x2_t u64x2)
  __arm_streaming __arm_inout("za")
{
  svadd_write_za64_vg1x2 (1, s32x2, s32x2); /* { dg-error {'svadd_write_za64_vg1x2' has no form that takes 'svint32x2_t' arguments} } */
  svadd_write_za64_vg1x2 (1, u32x2, u32x2); /* { dg-error {'svadd_write_za64_vg1x2' has no form that takes 'svuint32x2_t' arguments} } */
  svadd_write_za64_vg1x2 (1, s64x2, s64x2);
  svadd_write_za64_vg1x2 (1, u64x2, u64x2);
}
