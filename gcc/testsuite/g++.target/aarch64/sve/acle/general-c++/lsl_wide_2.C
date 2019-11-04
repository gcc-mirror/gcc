/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h"

void
f1 (svbool_t pg, svuint8_t x, svuint64_t y)
{
  svlsl_wide_x (pg, x); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svbool_t&, svuint8_t&\)'} } */
  svlsl_wide_x (pg, x, x, x, x); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svbool_t&, svuint8_t&, svuint8_t&, svuint8_t&, svuint8_t&\)'} } */
  svlsl_wide_x (x, x, y); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svuint8_t&, svuint8_t&, svuint64_t&\)'} } */
  svlsl_wide_x (pg, 1, y); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svbool_t&, int, svuint64_t&\)'} } */
  svlsl_wide_x (pg, x, x); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svbool_t&, svuint8_t&, svuint8_t&\)'} } */
  svlsl_wide_x (pg, y, y); /* { dg-error {no matching function for call to 'svlsl_wide_x\(svbool_t&, svuint64_t&, svuint64_t&\)'} } */
}
