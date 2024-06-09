/* { dg-do compile } */

#pragma GCC target "+sve2"

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svint16_t s16, svuint16_t u16,
    svfloat32_t f32, svint32_t s32, svuint32_t u32, svint32x2_t s32x2,
    svuint32x2_t u32x2)
{
  svqdmulh (s16); /* { dg-error {too few arguments to function 'svqdmulh'} } */
  svqdmulh (s16, s16, s16); /* { dg-error {too many arguments to function 'svqdmulh'} } */
  svqdmulh (pg, pg); /* { dg-error {'svqdmulh' has no form that takes 'svbool_t' arguments} } */
  svqdmulh (1, s16); /* { dg-error {passing 'int' to argument 1 of 'svqdmulh', which expects an SVE type rather than a scalar} } */
  svqdmulh (s16, s16);
  svqdmulh (s16, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svqdmulh', but argument 1 had type 'svint16_t'} } */
  svqdmulh (s16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svqdmulh', but argument 1 had type 'svint16_t'} } */
  svqdmulh (s16, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svqdmulh', but argument 1 had type 'svint16_t'} } */
  svqdmulh (s32, s32x2); /* { dg-error {passing tuple 'svint32x2_t' to argument 2 of 'svqdmulh' after passing single vector 'svint32_t' to argument 1} } */
  svqdmulh (s16, 0);
  svqdmulh (f16, f16); /* { dg-error {'svqdmulh' has no form that takes 'svfloat16_t' arguments} } */
  svqdmulh (u16, u16); /* { dg-error {'svqdmulh' has no form that takes 'svuint16_t' arguments} } */

  svqdmulh (s32x2, s32x2); /* { dg-error {ACLE function 'svqdmulh_s32_x2' can only be called when SME streaming mode is enabled} } */
}
