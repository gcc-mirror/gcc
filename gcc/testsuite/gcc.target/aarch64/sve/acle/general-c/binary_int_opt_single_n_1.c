/* { dg-do compile } */

#pragma GCC target "+sve2"

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svint16_t s16, svuint16_t u16,
    svfloat32_t f32, svint32_t s32, svuint32_t u32, svint32x2_t s32x2,
    svuint32x2_t u32x2)
{
  svrshl_x (pg, s16); /* { dg-error {too few arguments to function 'svrshl_x'} } */
  svrshl_x (pg, s16, s16, s16); /* { dg-error {too many arguments to function 'svrshl_x'} } */
  svrshl_x (s32, s16, s32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svrshl_x', which expects 'svbool_t'} } */
  svrshl_x (1, s16, s32); /* { dg-error {passing 'int' to argument 1 of 'svrshl_x', which expects 'svbool_t'} } */
  svrshl_x (pg, pg, s16); /* { dg-error {'svrshl_x' has no form that takes 'svbool_t' arguments} } */
  svrshl_x (pg, 1, s16); /* { dg-error {passing 'int' to argument 2 of 'svrshl_x', which expects an SVE type rather than a scalar} } */
  svrshl_x (pg, s16, s16);
  svrshl_x (pg, s16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, s16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, s16, s32); /* { dg-error {arguments 2 and 3 of 'svrshl_x' must have the same element size, but the values passed here have type 'svint16_t' and 'svint32_t' respectively} } */
  svrshl_x (pg, s16, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, s16, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, s16, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, s16, 0);
  svrshl_x (pg, f16, s16); /* { dg-error {'svrshl_x' has no form that takes 'svfloat16_t' arguments} } */
  svrshl_x (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, f16, s32); /* { dg-error {'svrshl_x' has no form that takes 'svfloat16_t' arguments} } */
  svrshl_x (pg, f16, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svrshl_x', which expects a vector of signed integers} } */
  svrshl_x (pg, u16, s16);

  svrshl_x (pg, s32x2, s32x2); /* { dg-error {'svrshl_x' has no form that takes 'svint32x2_t' arguments} } */
  svrshl_x (pg, s32x2, u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 3 of 'svrshl_x', which expects vectors of signed integers} } */
  svrshl_x (pg, s32x2, s32); /* { dg-error {'svrshl_x' has no form that takes 'svint32x2_t' arguments} } */
}
