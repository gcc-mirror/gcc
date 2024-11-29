/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2+fp8fma")

void
test (svfloat16_t f16, svmfloat8_t f8, fpm_t fpm, 
    svbool_t pg, svuint8_t u8, svuint16_t u16, svint32_t s32,
    svbfloat16_t bf16, svfloat32_t f32, svfloat64_t f64, mfloat8_t f)
{
  svmlalb_fpm (f16, f8, f8, fpm);
  svmlalt_fpm (f16, f8, f8, fpm);
  svmlalb_fpm (f16, f8, f, fpm);

  svmlalb_fpm (f16); /* { dg-error {too few arguments to function 'svmlalb_fpm'} } */
  svmlalb_fpm (f16, f8); /* { dg-error {too few arguments to function 'svmlalb_fpm'} } */
  svmlalb_fpm (f16, f8, f8); /* { dg-error {too few arguments to function 'svmlalb_fpm'} } */
  svmlalb_fpm (f8, f8, fpm); /* { dg-error {too few arguments to function 'svmlalb_fpm'} } */
  svmlalb_fpm (f16, f8, fpm); /* { dg-error {too few arguments to function 'svmlalb_fpm'} } */
  svmlalb_fpm (f16, f8, f8, fpm, 0); /* { dg-error {too many arguments to function 'svmlalb_fpm'} } */

  svmlalt_fpm (f32, f8, f8, fpm); /* { dg-error {'svmlalt_fpm' has no form that takes 'svfloat32_t' and 'svmfloat8_t' arguments} } */
  svmlalb_fpm (0, f8, f8, fpm); /* { dg-error {passing 'int' to argument 1 of 'svmlalb_fpm', which expects an SVE type rather than a scalar} } */
  svmlalb_fpm (pg, f8, f8, fpm); /* { dg-error {'svmlalb_fpm' has no form that takes 'svbool_t' and 'svmfloat8_t' arguments} } */
  svmlalb_fpm (u8, f8, f8, fpm); /* { dg-error {'svmlalb_fpm' has no form that takes 'svuint8_t' and 'svmfloat8_t' arguments} } */
  svmlalb_fpm (u16, f8, f8, fpm); /* { dg-error {'svmlalb_fpm' has no form that takes 'svuint16_t' and 'svmfloat8_t' arguments} } */
  svmlalb_fpm (f64, f8, f8, fpm); /* { dg-error {'svmlalb_fpm' has no form that takes 'svfloat64_t' and 'svmfloat8_t' arguments} } */
  svmlalb_fpm (f16, 0, f8, fpm); /* { dg-error {passing 'int' to argument 2 of 'svmlalb_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_fpm (f16, f16, f8, fpm); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svmlalb_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_fpm (f16, f8, 0, fpm); /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  svmlalb_fpm (f16, f8, f16, fpm); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmlalb_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_fpm (f16, f8, f8, f8); /* { dg-error {passing 'svmfloat8_t' to argument 4 of 'svmlalb_fpm', which expects 'uint64_t'} } */


  svmlallbb_fpm (f32, f8, f8, fpm);
  svmlallbt_fpm (f32, f8, f8, fpm);
  svmlalltb_fpm (f32, f8, f8, fpm);
  svmlalltt_fpm (f32, f8, f8, fpm);
  svmlallbb_fpm (f32, f8, f, fpm);

  svmlallbb_fpm (f16, f8, f8, fpm); /* { dg-error {'svmlallbb_fpm' has no form that takes 'svfloat16_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_fpm (f32); /* { dg-error {too few arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (f32, f8); /* { dg-error {too few arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (f32, f8, f8); /* { dg-error {too few arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (f8, f8, fpm); /* { dg-error {too few arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (f32, f8, fpm); /* { dg-error {too few arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (f32, f8, f8, fpm, 0); /* { dg-error {too many arguments to function 'svmlallbb_fpm'} } */
  svmlallbb_fpm (0, f8, f8, fpm); /* { dg-error {passing 'int' to argument 1 of 'svmlallbb_fpm', which expects an SVE type rather than a scalar} } */
  svmlallbb_fpm (pg, f8, f8, fpm); /* { dg-error {'svmlallbb_fpm' has no form that takes 'svbool_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_fpm (u8, f8, f8, fpm); /* { dg-error {'svmlallbb_fpm' has no form that takes 'svuint8_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_fpm (u16, f8, f8, fpm); /* { dg-error {'svmlallbb_fpm' has no form that takes 'svuint16_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_fpm (f64, f8, f8, fpm); /* { dg-error {'svmlallbb_fpm' has no form that takes 'svfloat64_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_fpm (f32, 0, f8, fpm); /* { dg-error {passing 'int' to argument 2 of 'svmlallbb_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_fpm (f32, f16, f8, fpm); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svmlallbb_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_fpm (f32, f8, 0, fpm); /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  svmlallbb_fpm (f32, f8, f16, fpm); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmlallbb_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_fpm (f32, f8, f8, f8); /* { dg-error {passing 'svmfloat8_t' to argument 4 of 'svmlallbb_fpm', which expects 'uint64_t'} } */
  
}
