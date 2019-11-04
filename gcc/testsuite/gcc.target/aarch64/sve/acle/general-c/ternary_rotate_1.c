/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat32_t f32, svfloat64_t f64, svint32_t s32, int i)
{
  svcmla_x (pg, f32, f32, f32); /* { dg-error {too few arguments to function 'svcmla_x'} } */
  svcmla_x (pg, f32, f32, f32, 90, 90); /* { dg-error {too many arguments to function 'svcmla_x'} } */
  svcmla_x (f32, f32, f32, f32, 90); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svcmla_x', which expects 'svbool_t'} } */
  svcmla_x (pg, pg, pg, pg, 90); /* { dg-error {'svcmla_x' has no form that takes 'svbool_t' arguments} } */
  svcmla_x (pg, s32, s32, s32, 90); /* { dg-error {'svcmla_x' has no form that takes 'svint32_t' arguments} } */
  svcmla_x (pg, 1, f32, f32, 90); /* { dg-error {passing 'int' to argument 2 of 'svcmla_x', which expects an SVE vector type} } */
  svcmla_x (pg, f32, 1, f32, 90); /* { dg-error {passing 'int' to argument 3 of 'svcmla_x', which expects an SVE vector type} } */
  svcmla_x (pg, f32, f32, 1, 90); /* { dg-error {passing 'int' to argument 4 of 'svcmla_x', which expects an SVE vector type} } */
  svcmla_x (pg, f32, f64, f32, 90); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svcmla_x', but previous arguments had type 'svfloat32_t'} } */
  svcmla_x (pg, f32, f32, f64, 90); /* { dg-error {passing 'svfloat64_t' to argument 4 of 'svcmla_x', but previous arguments had type 'svfloat32_t'} } */
  svcmla_x (pg, f32, f32, f32, s32); /* { dg-error {argument 5 of 'svcmla_x' must be an integer constant expression} } */
  svcmla_x (pg, f32, f32, f32, i); /* { dg-error {argument 5 of 'svcmla_x' must be an integer constant expression} } */
  svcmla_x (pg, f32, f32, f32, -90); /* { dg-error {passing -90 to argument 5 of 'svcmla_x', which expects 0, 90, 180 or 270} } */
  svcmla_x (pg, f32, f32, f32, 0);
  svcmla_x (pg, f32, f32, f32, 1); /* { dg-error {passing 1 to argument 5 of 'svcmla_x', which expects 0, 90, 180 or 270} } */
  svcmla_x (pg, f32, f32, f32, 90);
  svcmla_x (pg, f32, f32, f32, 180);
  svcmla_x (pg, f32, f32, f32, 270);
}
