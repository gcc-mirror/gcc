/* { dg-do compile } */

#include <arm_sve.h>

svuint8_t
f1 (svbool_t pg, int i, float f, double d, void *ptr, svfloat32_t f32,
    svint32_t i32)
{
  svadda (pg, f); /* { dg-error {too few arguments to function 'svadda'} } */
  svadda (pg, f, f32, f32); /* { dg-error {too many arguments to function 'svadda'} } */
  svadda (f32, f, f32); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svadda', which expects 'svbool_t'} } */
  svadda (pg, i, f32);
  svadda (pg, f, f32);
  svadda (pg, d, f32);
  svadda (pg, ptr, f32); /* { dg-error {incompatible type for argument 2 of 'svadda_f32'} } */
  svadda (pg, pg, f32); /* { dg-error {passing 'svbool_t' to argument 2 of 'svadda', which expects a scalar element} } */
  svadda (pg, f32, f32); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svadda', which expects a scalar element} } */
  svadda (pg, f, f); /* { dg-error {passing 'float' to argument 3 of 'svadda', which expects an SVE type rather than a scalar} } */
  svadda (pg, i, i32); /* { dg-error {'svadda' has no form that takes 'svint32_t' arguments} } */
  svadda (pg, i, i); /* { dg-error {passing 'int' to argument 3 of 'svadda', which expects an SVE type rather than a scalar} } */
}
