/* { dg-do compile } */

#pragma GCC target "+sme2"

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16x2_t f16x2, svint16x2_t s16x2, svuint16x2_t u16x2,
    svfloat32x2_t f32x2, svint32x2_t s32x2, svuint32x2_t u32x2,
    svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
    svfloat32_t f32)
  __arm_streaming
{
  svrshl (s16x2); /* { dg-error {too few arguments to function 'svrshl'} } */
  svrshl (s16x2, s16x2, s16x2); /* { dg-error {too many arguments to function 'svrshl'} } */
  svrshl (pg, s16x2); /* { dg-error {'svrshl' has no form that takes 'svbool_t' arguments} } */
  svrshl (1, s16x2); /* { dg-error {passing 'int' to argument 1 of 'svrshl', which expects an SVE type rather than a scalar} } */
  svrshl (s16, s16); /* { dg-error {'svrshl' has no form that takes 'svint16_t' arguments} } */
  svrshl (s16x2, s16x2);
  svrshl (s16x2, u16x2); /* { dg-error {passing 'svuint16x2_t' to argument 2 of 'svrshl', which expects vectors of signed integers} } */
  svrshl (s16x2, f16x2); /* { dg-error {passing 'svfloat16x2_t' to argument 2 of 'svrshl', which expects vectors of signed integers} } */
  svrshl (s16x2, s32x2); /* { dg-error {arguments 1 and 2 of 'svrshl' must have the same element size, but the values passed here have type 'svint16x2_t' and 'svint32x2_t' respectively} } */
  svrshl (s32x2, s16); /* { dg-error {arguments 1 and 2 of 'svrshl' must have the same element size, but the values passed here have type 'svint32x2_t' and 'svint16_t' respectively} } */
  svrshl (s32x2, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svrshl', which expects a vector of signed integers} } */
  svrshl (s32x2, s32);
  svrshl (s32x2, u32); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svrshl', which expects a vector of signed integers} } */
  svrshl (s32x2, f32); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svrshl', which expects a vector of signed integers} } */
  svrshl (s16x2, u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 2 of 'svrshl', which expects vectors of signed integers} } */
  svrshl (s16x2, f32x2); /* { dg-error {passing 'svfloat32x2_t' to argument 2 of 'svrshl', which expects vectors of signed integers} } */
  svrshl (s16x2, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svrshl', which expects a vector of signed integers} } */
  svrshl (s16x2, 0); /* { dg-error {passing 'int' to argument 2 of 'svrshl', which expects an SVE type rather than a scalar type} } */
  svrshl (f16x2, s16x2); /* { dg-error {'svrshl' has no form that takes 'svfloat16x2_t' arguments} } */
  svrshl (f16x2, u16x2); /* { dg-error {passing 'svuint16x2_t' to argument 2 of 'svrshl', which expects vectors of signed integers} } */
  svrshl (f16x2, s32x2); /* { dg-error {'svrshl' has no form that takes 'svfloat16x2_t' arguments} } */
  svrshl (u16x2, s16x2);
}
