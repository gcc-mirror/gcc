/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat32_t f32, svfloat64_t f64, svint32_t s32, int i)
{
  svcadd_x (pg, f32, f32); /* { dg-error {too few arguments to function 'svcadd_x'} } */
  svcadd_x (pg, f32, f32, 90, 90); /* { dg-error {too many arguments to function 'svcadd_x'} } */
  svcadd_x (f32, f32, f32, 90); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svcadd_x', which expects 'svbool_t'} } */
  svcadd_x (pg, pg, pg, 90); /* { dg-error {'svcadd_x' has no form that takes 'svbool_t' arguments} } */
  svcadd_x (pg, s32, s32, 90); /* { dg-error {'svcadd_x' has no form that takes 'svint32_t' arguments} } */
  svcadd_x (pg, 1, f32, 90); /* { dg-error {passing 'int' to argument 2 of 'svcadd_x', which expects an SVE type rather than a scalar} } */
  svcadd_x (pg, f32, 1, 90); /* { dg-error {passing 'int' to argument 3 of 'svcadd_x', which expects an SVE type rather than a scalar} } */
  svcadd_x (pg, f32, f64, 90); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svcadd_x', but argument 2 had type 'svfloat32_t'} } */
  svcadd_x (pg, f32, f32, s32); /* { dg-error {argument 4 of 'svcadd_x' must be an integer constant expression} } */
  svcadd_x (pg, f32, f32, i); /* { dg-error {argument 4 of 'svcadd_x' must be an integer constant expression} } */
  svcadd_x (pg, f32, f32, -90); /* { dg-error {passing -90 to argument 4 of 'svcadd_x', which expects either 90 or 270} } */
  svcadd_x (pg, f32, f32, 0); /* { dg-error {passing 0 to argument 4 of 'svcadd_x', which expects either 90 or 270} } */
  svcadd_x (pg, f32, f32, 1); /* { dg-error {passing 1 to argument 4 of 'svcadd_x', which expects either 90 or 270} } */
  svcadd_x (pg, f32, f32, 90);
  svcadd_x (pg, f32, f32, 180); /* { dg-error {passing 180 to argument 4 of 'svcadd_x', which expects either 90 or 270} } */
  svcadd_x (pg, f32, f32, 270);
}
