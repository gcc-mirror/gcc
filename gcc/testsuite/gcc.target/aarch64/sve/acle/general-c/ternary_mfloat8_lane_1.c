/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+ssve-fp8fma")

void
f1 (svfloat16_t f16, svmfloat8_t f8, fpm_t fpm, 
    svbool_t pg, svuint8_t u8, svuint16_t u16, svint32_t s32,
    svbfloat16_t bf16, svfloat32_t f32, svfloat64_t f64, mfloat8_t f, int i)
    __arm_streaming 
{
  svmlalb_lane_fpm (f16, f8, f8, 0, fpm);
  svmlalb_lane_fpm (f16, f8, f8, 7, fpm);
  svmlalb_lane_fpm (f16, f8, f8, 8, fpm);
  svmlalb_lane_fpm (f16, f8, f8, 15, fpm);

  svmlalb_lane_fpm (f16); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, f8); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, f8, 0); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, f8, fpm); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, 15, fpm); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f8, f8, 15, fpm); /* { dg-error {too few arguments to function 'svmlalb_lane_fpm'} } */

  svmlalb_lane_fpm (f16, f8, f8, 15, 0, fpm); /* { dg-error {too many arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, f8, 15, fpm, fpm); /* { dg-error {too many arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f8, f8, f8, 15, fpm); /* { dg-error {too many arguments to function 'svmlalb_lane_fpm'} } */
  svmlalb_lane_fpm (f16, f16, f8, f8, 15, fpm); /* { dg-error {too many arguments to function 'svmlalb_lane_fpm'} } */

  svmlalb_lane_fpm (f32, bf16, bf16, 0, fpm); /* { dg-error {passing 'svbfloat16_t' to argument 2 of 'svmlalb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_lane_fpm (0, f8, f8, 0, fpm); /* { dg-error {passing 'int' to argument 1 of 'svmlalb_lane_fpm', which expects an SVE type rather than a scalar} } */
  svmlalb_lane_fpm (pg, f8, f8, 0, fpm); /* { dg-error {'svmlalb_lane_fpm' has no form that takes 'svbool_t' and 'svmfloat8_t' arguments} } */
  svmlalb_lane_fpm (u8, f8, f8, 0, fpm); /* { dg-error {'svmlalb_lane_fpm' has no form that takes 'svuint8_t' and 'svmfloat8_t' arguments} } */
  svmlalb_lane_fpm (u16, f8, f8, 0, fpm); /* { dg-error {'svmlalb_lane_fpm' has no form that takes 'svuint16_t' and 'svmfloat8_t' arguments} } */
  svmlalb_lane_fpm (f32, f8, f8, 0, fpm); /* { dg-error {'svmlalb_lane_fpm' has no form that takes 'svfloat32_t' and 'svmfloat8_t' arguments} } */
  svmlalb_lane_fpm (f64, f8, f8, 0, fpm); /* { dg-error {'svmlalb_lane_fpm' has no form that takes 'svfloat64_t' and 'svmfloat8_t' arguments} } */
  svmlalb_lane_fpm (f16, 0, f8, 0, fpm); /* { dg-error {passing 'int' to argument 2 of 'svmlalb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_lane_fpm (f16, f32, f8, 0, fpm); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svmlalb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_lane_fpm (f16, f8, 0, 0, fpm); /* { dg-error {passing 'int' to argument 3 of 'svmlalb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlalb_lane_fpm (f16, f8, f32, 0, fpm); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svmlalb_lane_fpm', which expects 'svmfloat8_t'} } */

  svmlalb_lane_fpm (f16, f8, f8, s32, fpm); /* { dg-error {argument 4 of 'svmlalb_lane_fpm' must be an integer constant expression} } */
  svmlalb_lane_fpm (f16, f8, f8, i, fpm); /* { dg-error {argument 4 of 'svmlalb_lane_fpm' must be an integer constant expression} } */
  svmlalb_lane_fpm (f16, f8, f8, 16, fpm); /* { dg-error {passing 16 to argument 4 of 'svmlalb_lane_fpm', which expects a value in the range \[0, 15\]} } */
  svmlalb_lane_fpm (f16, f8, f8, -1, fpm); /* { dg-error {passing -1 to argument 4 of 'svmlalb_lane_fpm', which expects a value in the range \[0, 15\]} } */
  svmlalb_lane_fpm (f16, f8, f8, 15, f8); /* { dg-error {passing 'svmfloat8_t' to argument 5 of 'svmlalb_lane_fpm', which expects 'uint64_t'} } */


  svmlallbb_lane_fpm (f32, f8, f8, 0, fpm);
  svmlallbb_lane_fpm (f32, f8, f8, 7, fpm);
  svmlallbb_lane_fpm (f32, f8, f8, 8, fpm);
  svmlallbb_lane_fpm (f32, f8, f8, 15, fpm);

  svmlallbb_lane_fpm (f32); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, f8); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, f8, 0); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, f8, fpm); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, 15, fpm); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f8, f8, 15, fpm); /* { dg-error {too few arguments to function 'svmlallbb_lane_fpm'} } */

  svmlallbb_lane_fpm (f32, f8, f8, 15, 0, fpm); /* { dg-error {too many arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, f8, 15, fpm, fpm); /* { dg-error {too many arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f8, f8, f8, 15, fpm); /* { dg-error {too many arguments to function 'svmlallbb_lane_fpm'} } */
  svmlallbb_lane_fpm (f32, f16, f8, f8, 15, fpm); /* { dg-error {too many arguments to function 'svmlallbb_lane_fpm'} } */

  svmlallbb_lane_fpm (f32, bf16, bf16, 0, fpm); /* { dg-error {passing 'svbfloat16_t' to argument 2 of 'svmlallbb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_lane_fpm (0, f8, f8, 0, fpm); /* { dg-error {passing 'int' to argument 1 of 'svmlallbb_lane_fpm', which expects an SVE type rather than a scalar} } */
  svmlallbb_lane_fpm (pg, f8, f8, 0, fpm); /* { dg-error {'svmlallbb_lane_fpm' has no form that takes 'svbool_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_lane_fpm (u8, f8, f8, 0, fpm); /* { dg-error {'svmlallbb_lane_fpm' has no form that takes 'svuint8_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_lane_fpm (u16, f8, f8, 0, fpm); /* { dg-error {'svmlallbb_lane_fpm' has no form that takes 'svuint16_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_lane_fpm (f16, f8, f8, 0, fpm); /* { dg-error {'svmlallbb_lane_fpm' has no form that takes 'svfloat16_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_lane_fpm (f64, f8, f8, 0, fpm); /* { dg-error {'svmlallbb_lane_fpm' has no form that takes 'svfloat64_t' and 'svmfloat8_t' arguments} } */
  svmlallbb_lane_fpm (f32, 0, f8, 0, fpm); /* { dg-error {passing 'int' to argument 2 of 'svmlallbb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_lane_fpm (f32, f32, f8, 0, fpm); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svmlallbb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_lane_fpm (f32, f8, 0, 0, fpm); /* { dg-error {passing 'int' to argument 3 of 'svmlallbb_lane_fpm', which expects 'svmfloat8_t'} } */
  svmlallbb_lane_fpm (f32, f8, f32, 0, fpm); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svmlallbb_lane_fpm', which expects 'svmfloat8_t'} } */

  svmlallbb_lane_fpm (f32, f8, f8, s32, fpm); /* { dg-error {argument 4 of 'svmlallbb_lane_fpm' must be an integer constant expression} } */
  svmlallbb_lane_fpm (f32, f8, f8, i, fpm); /* { dg-error {argument 4 of 'svmlallbb_lane_fpm' must be an integer constant expression} } */
  svmlallbb_lane_fpm (f32, f8, f8, 16, fpm); /* { dg-error {passing 16 to argument 4 of 'svmlallbb_lane_fpm', which expects a value in the range \[0, 15\]} } */
  svmlallbb_lane_fpm (f32, f8, f8, -1, fpm); /* { dg-error {passing -1 to argument 4 of 'svmlallbb_lane_fpm', which expects a value in the range \[0, 15\]} } */
}
