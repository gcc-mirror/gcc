/* { dg-do compile } */

#pragma GCC target "+sve2+sme2"

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16x2_t f16x2, svint16x2_t s16x2, svuint16x2_t u16x2,
    svfloat32x2_t f32x2, svint32x2_t s32x2, svuint32x2_t u32x2,
    svint32x3_t s32x3, svint32x4_t s32x4,
    svint16_t s16, svuint16_t u16, svfloat16_t f16, svint32_t s32,
    svuint32_t u32, svfloat32_t f32)
  __arm_streaming
{
  svadd (s16x2); /* { dg-error {too few arguments to function 'svadd'} } */
  svadd (s16x2, s16x2, s16x2); /* { dg-error {too many arguments to function 'svadd'} } */
  svadd (pg, s16x2); /* { dg-error {passing 'svint16x2_t' to argument 2 of 'svadd', which expects a single SVE vector rather than a tuple} } */
  svadd (1, s16x2); /* { dg-error {passing 'int' to argument 1 of 'svadd', which expects an SVE type rather than a scalar} } */
  svadd (s16, s16); /* { dg-error {'svadd' has no form that takes 'svint16_t' arguments} } */
  svadd (s16x2, s16x2); /* { dg-error {passing 'svint16x2_t' to argument 2 of 'svadd', which expects a single SVE vector rather than a tuple} } */
  svadd (s16x2, u16x2); /* { dg-error {passing 'svuint16x2_t' to argument 2 of 'svadd', which expects a single SVE vector rather than a tuple} } */
  svadd (s16x2, s16);
  svadd (s16x2, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svadd', but argument 1 was a tuple of 'svint16_t'} } */
  svadd (s16x2, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svadd', but argument 1 was a tuple of 'svint16_t'} } */
  svadd (s32x2, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svadd', but argument 1 was a tuple of 'svint32_t'} } */
  svadd (s32x2, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svadd', but argument 1 was a tuple of 'svint32_t'} } */
  svadd (s32x2, s32);
  svadd (s32x3, s32); /* { dg-error {'svadd' has no form that takes 'svint32x3_t' arguments} } */
  svadd (s32x4, s32x2); /* { dg-error {passing 'svint32x2_t' to argument 2 of 'svadd', which expects a single SVE vector rather than a tuple} } */
  svadd (f32x2, f32); /* { dg-error {'svadd' has no form that takes 'svfloat32x2_t' arguments} } */

  svadd (s16x2, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svadd', but argument 1 was a tuple of 'svint16_t'} } */
  svadd (s16x2, 0); /* { dg-error {passing 'int' to argument 2 of 'svadd', which expects an SVE type rather than a scalar type} } */
}
