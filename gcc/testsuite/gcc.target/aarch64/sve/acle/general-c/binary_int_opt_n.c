/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svint16_t s16, svuint16_t u16,
    svfloat32_t f32, svint32_t s32, svuint32_t u32)
{
  svscale_x (pg, f16); /* { dg-error {too few arguments to function 'svscale_x'} } */
  svscale_x (pg, f16, s16, s16); /* { dg-error {too many arguments to function 'svscale_x'} } */
  svscale_x (s32, f16, s32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svscale_x', which expects 'svbool_t'} } */
  svscale_x (1, f16, s32); /* { dg-error {passing 'int' to argument 1 of 'svscale_x', which expects 'svbool_t'} } */
  svscale_x (pg, pg, s16); /* { dg-error {'svscale_x' has no form that takes 'svbool_t' arguments} } */
  svscale_x (pg, 1, s16); /* { dg-error {passing 'int' to argument 2 of 'svscale_x', which expects an SVE vector type} } */
  svscale_x (pg, f16, s16);
  svscale_x (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, f16, s32); /* { dg-error {arguments 2 and 3 of 'svscale_x' must have the same element size, but the values passed here have type 'svfloat16_t' and 'svint32_t' respectively} } */
  svscale_x (pg, f16, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, f16, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, f16, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, f16, 0);
  svscale_x (pg, s16, s16); /* { dg-error {'svscale_x' has no form that takes 'svint16_t' arguments} } */
  svscale_x (pg, s16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, s16, s32); /* { dg-error {'svscale_x' has no form that takes 'svint16_t' arguments} } */
  svscale_x (pg, s16, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svscale_x', which expects a vector of signed integers} } */
  svscale_x (pg, u16, s16); /* { dg-error {'svscale_x' has no form that takes 'svuint16_t' arguments} } */
}
