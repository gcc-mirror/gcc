/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svfloat32_t f32, svfloat64_t f64, svint32_t s32, int i)
{
  svtmad (f32, f32); /* { dg-error {too few arguments to function 'svtmad'} } */
  svtmad (f32, f32, 0, 0); /* { dg-error {too many arguments to function 'svtmad'} } */
  svtmad (pg, pg, 0); /* { dg-error {'svtmad' has no form that takes 'svbool_t' arguments} } */
  svtmad (s32, s32, 0); /* { dg-error {'svtmad' has no form that takes 'svint32_t' arguments} } */
  svtmad (1, f32, 0); /* { dg-error {passing 'int' to argument 1 of 'svtmad', which expects an SVE type rather than a scalar} } */
  svtmad (f32, 1, 0); /* { dg-error {passing 'int' to argument 2 of 'svtmad', which expects an SVE type rather than a scalar} } */
  svtmad (f32, f64, 0); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svtmad', but argument 1 had type 'svfloat32_t'} } */
  svtmad (f32, f32, s32); /* { dg-error {argument 3 of 'svtmad' must be an integer constant expression} } */
  svtmad (f32, f32, i); /* { dg-error {argument 3 of 'svtmad' must be an integer constant expression} } */
  svtmad (f32, f32, -1); /* { dg-error {passing -1 to argument 3 of 'svtmad', which expects a value in the range \[0, 7\]} } */
  svtmad (f32, f32, 0);
  svtmad (f32, f32, 1);
  svtmad (f32, f32, 7);
  svtmad (f32, f32, 8); /* { dg-error {passing 8 to argument 3 of 'svtmad', which expects a value in the range \[0, 7\]} } */
}
