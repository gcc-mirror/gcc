/* { dg-do compile } */

#include <arm_sve.h>

svuint8_t
f1 (svbool_t pg, svuint8_t x, svint8_t w, svuint64_t y)
{
  svlsl_wide_u8_x (pg, x, x); /* { dg-error "cannot convert 'svuint8_t' to 'svuint64_t'" } */
  svlsl_wide_u8_x (pg, x); /* { dg-error {too few arguments to function 'svuint8_t svlsl_wide_u8_x\(svbool_t, svuint8_t, svuint64_t\)'} } */
  svlsl_wide_u8_x (pg, x, y, x); /* { dg-error {too many arguments to function 'svuint8_t svlsl_wide_u8_x\(svbool_t, svuint8_t, svuint64_t\)'} } */
  return svlsl_wide_s8_x (pg, w, y); /* { dg-error {cannot convert 'svint8_t' to 'svuint8_t' in return} } */
}
