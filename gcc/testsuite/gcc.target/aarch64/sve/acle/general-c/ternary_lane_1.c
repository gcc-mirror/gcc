/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svfloat32_t f32, svfloat64_t f64,
    svint32_t s32, int i)
{
  svmla_lane (f32, f32, f32); /* { dg-error {too few arguments to function 'svmla_lane'} } */
  svmla_lane (f32, f32, f32, 0, 0); /* { dg-error {too many arguments to function 'svmla_lane'} } */
  svmla_lane (pg, pg, pg, 0); /* { dg-error {'svmla_lane' has no form that takes 'svbool_t' arguments} } */
  svmla_lane (s32, s32, s32, 0); /* { dg-error {ACLE function 'svmla_lane_s32' requires ISA extension 'sve2'} "" { xfail aarch64_sve2 } } */
  svmla_lane (1, f32, f32, 0); /* { dg-error {passing 'int' to argument 1 of 'svmla_lane', which expects an SVE type rather than a scalar} } */
  svmla_lane (f32, 1, f32, 0); /* { dg-error {passing 'int' to argument 2 of 'svmla_lane', which expects an SVE type rather than a scalar} } */
  svmla_lane (f32, f32, 1, 0); /* { dg-error {passing 'int' to argument 3 of 'svmla_lane', which expects an SVE type rather than a scalar} } */
  svmla_lane (f32, f64, f32, 0); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svmla_lane', but argument 1 had type 'svfloat32_t'} } */
  svmla_lane (f32, f32, f64, 0); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svmla_lane', but argument 1 had type 'svfloat32_t'} } */
  svmla_lane (f32, f32, f32, s32); /* { dg-error {argument 4 of 'svmla_lane' must be an integer constant expression} } */
  svmla_lane (f32, f32, f32, i); /* { dg-error {argument 4 of 'svmla_lane' must be an integer constant expression} } */

  svmla_lane (f16, f16, f16, 0);
  svmla_lane (f16, f16, f16, 7);
  svmla_lane (f16, f16, f16, 8); /* { dg-error {passing 8 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 7\]} } */
  svmla_lane (f16, f16, f16, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 7\]} } */

  svmla_lane (f32, f32, f32, 0);
  svmla_lane (f32, f32, f32, 3);
  svmla_lane (f32, f32, f32, 4); /* { dg-error {passing 4 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 3\]} } */
  svmla_lane (f32, f32, f32, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 3\]} } */

  svmla_lane (f64, f64, f64, 0);
  svmla_lane (f64, f64, f64, 1);
  svmla_lane (f64, f64, f64, 2); /* { dg-error {passing 2 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 1\]} } */
  svmla_lane (f64, f64, f64, -1); /* { dg-error {passing -1 to argument 4 of 'svmla_lane', which expects a value in the range \[0, 1\]} } */
}
