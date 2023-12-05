/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svlogb_m (s32, pg); /* { dg-error {too few arguments to function 'svlogb_m'} } */
  svlogb_m (s32, pg, f32, s32); /* { dg-error {too many arguments to function 'svlogb_m'} } */
  svlogb_m (0, pg, f32); /* { dg-error {passing 'int' to argument 1 of 'svlogb_m', which expects an SVE type rather than a scalar} } */
  svlogb_m (s32, u32, f32); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svlogb_m', which expects 'svbool_t'} } */
  svlogb_m (s32, 0, f32); /* { dg-error {passing 'int' to argument 2 of 'svlogb_m', which expects 'svbool_t'} } */
  svlogb_m (s32, pg, s32); /* { dg-error {'svlogb_m' has no form that takes 'svint32_t' arguments} } */
  svlogb_m (s32, pg, u32); /* { dg-error {'svlogb_m' has no form that takes 'svuint32_t' arguments} } */
  svlogb_m (s32, pg, f32);
  svlogb_m (s32, pg, pg); /* { dg-error {'svlogb_m' has no form that takes 'svbool_t' arguments} } */

  svlogb_m (pg, pg, f32); /* { dg-error {passing 'svbool_t' to argument 1 of 'svlogb_m', which expects a vector of signed integers} } */
  svlogb_m (u32, pg, f32); /* { dg-error {passing 'svuint32_t' to argument 1 of 'svlogb_m', which expects a vector of signed integers} } */
  svlogb_m (f32, pg, f32); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svlogb_m', which expects a vector of signed integers} } */
  svlogb_m (u64, pg, f32); /* { dg-error {passing 'svuint64_t' to argument 1 of 'svlogb_m', which expects a vector of signed integers} } */
  svlogb_m (s64, pg, f32); /* { dg-error {arguments 1 and 3 of 'svlogb_m' must have the same element size, but the values passed here have type 'svint64_t' and 'svfloat32_t' respectively} } */
  svlogb_m (s32, pg, f64); /* { dg-error {arguments 1 and 3 of 'svlogb_m' must have the same element size, but the values passed here have type 'svint32_t' and 'svfloat64_t' respectively} } */
}
