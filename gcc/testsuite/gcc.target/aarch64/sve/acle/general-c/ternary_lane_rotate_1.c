/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat16_t f16, svfloat32_t f32, svfloat64_t f64,
    svint32_t s32, int i)
{
  svcmla_lane (f32, f32, f32, 0); /* { dg-error {too few arguments to function 'svcmla_lane'} } */
  svcmla_lane (f32, f32, f32, 0, 90, 90); /* { dg-error {too many arguments to function 'svcmla_lane'} } */
  svcmla_lane (pg, pg, pg, 0, 90); /* { dg-error {'svcmla_lane' has no form that takes 'svbool_t' arguments} } */
  svcmla_lane (s32, s32, s32, 0, 90); /* { dg-error {ACLE function 'svcmla_lane_s32' requires ISA extension 'sve2'} "" { xfail aarch64_sve2 } } */
  svcmla_lane (f64, f64, f64, 0, 90); /* { dg-error {'svcmla_lane' has no form that takes 'svfloat64_t' arguments} } */
  svcmla_lane (1, f32, f32, 0, 90); /* { dg-error {passing 'int' to argument 1 of 'svcmla_lane', which expects an SVE type rather than a scalar} } */
  svcmla_lane (f32, 1, f32, 0, 90); /* { dg-error {passing 'int' to argument 2 of 'svcmla_lane', which expects an SVE type rather than a scalar} } */
  svcmla_lane (f32, f32, 1, 0, 90); /* { dg-error {passing 'int' to argument 3 of 'svcmla_lane', which expects an SVE type rather than a scalar} } */
  svcmla_lane (f32, f64, f32, 0, 90); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svcmla_lane', but argument 1 had type 'svfloat32_t'} } */
  svcmla_lane (f32, f32, f64, 0, 90); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svcmla_lane', but argument 1 had type 'svfloat32_t'} } */
  svcmla_lane (f32, f32, f32, s32, 0); /* { dg-error {argument 4 of 'svcmla_lane' must be an integer constant expression} } */
  svcmla_lane (f32, f32, f32, i, 0); /* { dg-error {argument 4 of 'svcmla_lane' must be an integer constant expression} } */

  svcmla_lane (f16, f16, f16, 0, 0);
  svcmla_lane (f16, f16, f16, 3, 0);
  svcmla_lane (f16, f16, f16, 4, 0); /* { dg-error {passing 4 to argument 4 of 'svcmla_lane', which expects a value in the range \[0, 3\]} } */
  svcmla_lane (f16, f16, f16, -1, 0); /* { dg-error {passing -1 to argument 4 of 'svcmla_lane', which expects a value in the range \[0, 3\]} } */

  svcmla_lane (f32, f32, f32, 0, 0);
  svcmla_lane (f32, f32, f32, 1, 0);
  svcmla_lane (f32, f32, f32, 2, 0); /* { dg-error {passing 2 to argument 4 of 'svcmla_lane', which expects a value in the range \[0, 1\]} } */
  svcmla_lane (f32, f32, f32, -1, 0); /* { dg-error {passing -1 to argument 4 of 'svcmla_lane', which expects a value in the range \[0, 1\]} } */

  svcmla_lane (f32, f32, f32, 0, -90); /* { dg-error {passing -90 to argument 5 of 'svcmla_lane', which expects 0, 90, 180 or 270} } */
  svcmla_lane (f32, f32, f32, 0, 0);
  svcmla_lane (f32, f32, f32, 0, 1); /* { dg-error {passing 1 to argument 5 of 'svcmla_lane', which expects 0, 90, 180 or 270} } */
  svcmla_lane (f32, f32, f32, 0, 90);
  svcmla_lane (f32, f32, f32, 0, 180);
  svcmla_lane (f32, f32, f32, 0, 270);
}
