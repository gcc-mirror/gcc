/* { dg-do compile } */

#pragma GCC target "+sve2+sme2"

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16x2_t f16x2, svint16x2_t s16x2, svuint16x2_t u16x2,
    svfloat32x2_t f32x2, svint32x2_t s32x2, svuint32x2_t u32x2,
    svint32x3_t s32x3, svint32x4_t s32x4,
    svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
    svfloat32_t f32)
  __arm_streaming
{
  svqdmulh (s16x2); /* { dg-error {too few arguments to function 'svqdmulh'} } */
  svqdmulh (s16x2, s16x2, s16x2); /* { dg-error {too many arguments to function 'svqdmulh'} } */
  svqdmulh (pg, s16x2); /* { dg-error {'svqdmulh' has no form that takes 'svbool_t' arguments} } */
  svqdmulh (1, s16x2); /* { dg-error {passing 'int' to argument 1 of 'svqdmulh', which expects an SVE type rather than a scalar} } */
  svqdmulh (s16, s16);
  svqdmulh (s16x2, s16x2);
  svqdmulh (s16x2, u16x2); /* { dg-error {passing 'svuint16x2_t' to argument 2 of 'svqdmulh', but argument 1 had type 'svint16x2_t'} } */
  svqdmulh (s16x2, f16x2); /* { dg-error {passing 'svfloat16x2_t' to argument 2 of 'svqdmulh', but argument 1 had type 'svint16x2_t'} } */
  svqdmulh (s32x2, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svqdmulh', but argument 1 was a tuple of 'svint32_t'} } */
  svqdmulh (s32x2, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svqdmulh', but argument 1 was a tuple of 'svint32_t'} } */
  svqdmulh (s32x2, s32);
  svqdmulh (s32x2, s32x3); /* { dg-error {passing mismatched tuple types 'svint32x2_t' and 'svint32x3_t' to arguments 1 and 2 of 'svqdmulh'} } */
  svqdmulh (s32x2, s32x4); /* { dg-error {passing mismatched tuple types 'svint32x2_t' and 'svint32x4_t' to arguments 1 and 2 of 'svqdmulh'} } */
  svqdmulh (s32x3, s32x2); /* { dg-error {'svqdmulh' has no form that takes 'svint32x3_t' arguments} } */
  svqdmulh (s32x3, s32x3); /* { dg-error {'svqdmulh' has no form that takes 'svint32x3_t' arguments} } */
  svqdmulh (s32x4, s32x2); /* { dg-error {passing mismatched tuple types 'svint32x4_t' and 'svint32x2_t' to arguments 1 and 2 of 'svqdmulh'} } */
  svqdmulh (s32x4, s32x3); /* { dg-error {passing mismatched tuple types 'svint32x4_t' and 'svint32x3_t' to arguments 1 and 2 of 'svqdmulh'} } */
  svqdmulh (s32x4, s32x4);
  svqdmulh (u32x2, u32x2); /* { dg-error {'svqdmulh' has no form that takes 'svuint32x2_t' arguments} } */
  svqdmulh (u32x2, u32); /* { dg-error {'svqdmulh' has no form that takes 'svuint32x2_t' arguments} } */

  svqdmulh (s16x2, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svqdmulh', but argument 1 was a tuple of 'svint16_t'} } */
  svqdmulh (s16x2, 0); /* { dg-error {passing 'int' to argument 2 of 'svqdmulh', which expects an SVE type rather than a scalar type} } */
}
