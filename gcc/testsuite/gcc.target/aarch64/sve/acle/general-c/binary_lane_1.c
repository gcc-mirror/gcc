/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svfloat32_t f32, svfloat64_t f64,
    svint32_t s32, int i)
{
  svmul_lane (f32, f32); /* { dg-error {too few arguments to function 'svmul_lane'} } */
  svmul_lane (f32, f32, 0, 0); /* { dg-error {too many arguments to function 'svmul_lane'} } */
  svmul_lane (pg, pg, 0); /* { dg-error {'svmul_lane' has no form that takes 'svbool_t' arguments} } */
  svmul_lane (s32, s32, 0); /* { dg-error {'svmul_lane' has no form that takes 'svint32_t' arguments} } */
  svmul_lane (1, f32, 0); /* { dg-error {passing 'int' to argument 1 of 'svmul_lane', which expects an SVE vector type} } */
  svmul_lane (f32, 1, 0); /* { dg-error {passing 'int' to argument 2 of 'svmul_lane', which expects an SVE vector type} } */
  svmul_lane (f32, f64, 0); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svmul_lane', but previous arguments had type 'svfloat32_t'} } */
  svmul_lane (f32, f32, s32); /* { dg-error {argument 3 of 'svmul_lane' must be an integer constant expression} } */
  svmul_lane (f32, f32, i); /* { dg-error {argument 3 of 'svmul_lane' must be an integer constant expression} } */

  svmul_lane (f16, f16, 0);
  svmul_lane (f16, f16, 7);
  svmul_lane (f16, f16, 8); /* { dg-error {passing 8 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 7\]} } */
  svmul_lane (f16, f16, -1); /* { dg-error {passing -1 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 7\]} } */

  svmul_lane (f32, f32, 0);
  svmul_lane (f32, f32, 3);
  svmul_lane (f32, f32, 4); /* { dg-error {passing 4 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 3\]} } */
  svmul_lane (f32, f32, -1); /* { dg-error {passing -1 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 3\]} } */

  svmul_lane (f64, f64, 0);
  svmul_lane (f64, f64, 1);
  svmul_lane (f64, f64, 2); /* { dg-error {passing 2 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 1\]} } */
  svmul_lane (f64, f64, -1); /* { dg-error {passing -1 to argument 3 of 'svmul_lane', which expects a value in the range \[0, 1\]} } */
}
