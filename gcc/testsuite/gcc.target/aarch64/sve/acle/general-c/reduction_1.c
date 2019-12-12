/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svuint32x2_t u32x2)
{
  svorv (pg); /* { dg-error {too few arguments to function 'svorv'} } */
  svorv (pg, u32, u32); /* { dg-error {too many arguments to function 'svorv'} } */
  svorv (0, u32); /* { dg-error {passing 'int' to argument 1 of 'svorv', which expects 'svbool_t'} } */
  svorv (u32, u32); /* { dg-error {passing 'svuint32_t' to argument 1 of 'svorv', which expects 'svbool_t'} } */
  svorv (pg, 0); /* { dg-error {passing 'int' to argument 2 of 'svorv', which expects an SVE vector type} } */
  svorv (pg, pg); /* { dg-error {'svorv' has no form that takes 'svbool_t' arguments} } */
  svorv (pg, s32);
  svorv (pg, u32);
  svorv (pg, f32); /* { dg-error {'svorv' has no form that takes 'svfloat32_t' arguments} } */
  svorv (pg, u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 2 of 'svorv', which expects a single SVE vector rather than a tuple} } */
}
