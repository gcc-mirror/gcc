/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8, svuint64_t u64)
{
  svlsl_wide_x (pg, u8); /* { dg-error {too few arguments to function 'svlsl_wide_x'} } */
  svlsl_wide_x (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svlsl_wide_x'} } */
  svlsl_wide_x (u8, u8, u64); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svlsl_wide_x', which expects 'svbool_t'} } */
  svlsl_wide_x (pg, 1, u64); /* { dg-error {passing 'int' to argument 2 of 'svlsl_wide_x', which expects an SVE type rather than a scalar} } */
  svlsl_wide_x (pg, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svlsl_wide_x', which expects 'svuint64_t'} } */
  svlsl_wide_x (pg, u64, u64); /* { dg-error {'svlsl_wide_x' has no form that takes 'svuint64_t' arguments} } */
}
