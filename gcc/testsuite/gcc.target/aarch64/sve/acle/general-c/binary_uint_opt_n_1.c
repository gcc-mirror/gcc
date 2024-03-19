/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svint16_t s16, svuint16_t u16,
    svfloat32_t f32, svint32_t s32, svuint32_t u32)
{
  svlsl_x (pg, s16); /* { dg-error {too few arguments to function 'svlsl_x'} } */
  svlsl_x (pg, s16, u16, u16); /* { dg-error {too many arguments to function 'svlsl_x'} } */
  svlsl_x (s32, s32, u32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svlsl_x', which expects 'svbool_t'} } */
  svlsl_x (1, s32, u32); /* { dg-error {passing 'int' to argument 1 of 'svlsl_x', which expects 'svbool_t'} } */
  svlsl_x (pg, pg, u16); /* { dg-error {'svlsl_x' has no form that takes 'svbool_t' arguments} } */
  svlsl_x (pg, 1, s16); /* { dg-error {passing 'int' to argument 2 of 'svlsl_x', which expects an SVE type rather than a scalar} } */
  svlsl_x (pg, s16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, s16, u16);
  svlsl_x (pg, s16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, s16, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, s16, u32); /* { dg-error {arguments 2 and 3 of 'svlsl_x' must have the same element size, but the values passed here have type 'svint16_t' and 'svuint32_t' respectively} } */
  svlsl_x (pg, s16, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, s16, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, s16, 0);
  svlsl_x (pg, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, f16, u16); /* { dg-error {'svlsl_x' has no form that takes 'svfloat16_t' arguments} } */
  svlsl_x (pg, f16, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svlsl_x', which expects a vector of unsigned integers} } */
  svlsl_x (pg, f16, u32); /* { dg-error {'svlsl_x' has no form that takes 'svfloat16_t' arguments} } */
}
