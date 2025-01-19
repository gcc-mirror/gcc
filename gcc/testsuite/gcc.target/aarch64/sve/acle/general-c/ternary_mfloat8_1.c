/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2+fp8dot2")

void
test (svfloat16_t f16, svmfloat8_t f8, fpm_t fpm, 
    svbool_t pg, svuint8_t u8, svuint16_t u16, svint32_t s32,
    svbfloat16_t bf16, svfloat32_t f32, svfloat64_t f64, mfloat8_t f)
{
  svdot_fpm (f16, f8, f8, fpm);
  svdot_fpm (f32, f8, f8, fpm);

  svdot_fpm (f16); /* { dg-error {too few arguments to function 'svdot_fpm'} } */
  svdot_fpm (f16, f8); /* { dg-error {too few arguments to function 'svdot_fpm'} } */
  svdot_fpm (f16, f8, f8); /* { dg-error {too few arguments to function 'svdot_fpm'} } */
  svdot_fpm (f8, f8, fpm); /* { dg-error {too few arguments to function 'svdot_fpm'} } */
  svdot_fpm (f16, f8, fpm); /* { dg-error {too few arguments to function 'svdot_fpm'} } */
  svdot_fpm (f16, f8, f8, fpm, 0); /* { dg-error {too many arguments to function 'svdot_fpm'} } */

  svdot_fpm (0, f8, f8, fpm); /* { dg-error {passing 'int' to argument 1 of 'svdot_fpm', which expects an SVE type rather than a scalar} } */
  svdot_fpm (f16, f8, f, fpm); /* { dg-error {passing 'mfloat8_t' {aka '__mfp8'} to argument 3 of 'svdot_fpm', which expects 'svmfloat8_t'} } */
  svdot_fpm (pg, f8, f8, fpm); /* { dg-error {'svdot_fpm' has no form that takes 'svbool_t' and 'svmfloat8_t' arguments} } */
  svdot_fpm (u8, f8, f8, fpm); /* { dg-error {'svdot_fpm' has no form that takes 'svuint8_t' and 'svmfloat8_t' arguments} } */
  svdot_fpm (u16, f8, f8, fpm); /* { dg-error {'svdot_fpm' has no form that takes 'svuint16_t' and 'svmfloat8_t' arguments} } */
  svdot_fpm (f64, f8, f8, fpm); /* { dg-error {'svdot_fpm' has no form that takes 'svfloat64_t' and 'svmfloat8_t' arguments} } */
  svdot_fpm (f16, 0, f8, fpm); /* { dg-error {passing 'int' to argument 2 of 'svdot_fpm', which expects 'svmfloat8_t'} } */
  svdot_fpm (f16, f16, f8, fpm); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svdot_fpm', which expects 'svmfloat8_t'} } */
  svdot_fpm (f16, f8, 0, fpm); /* { dg-error {passing 'int' to argument 3 of 'svdot_fpm', which expects 'svmfloat8_t'} } */
  svdot_fpm (f16, f8, f16, fpm); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svdot_fpm', which expects 'svmfloat8_t'} } */
  svdot_fpm (f16, f8, f8, f8); /* { dg-error {passing 'svmfloat8_t' to argument 4 of 'svdot_fpm', which expects 'uint64_t'} } */
}
