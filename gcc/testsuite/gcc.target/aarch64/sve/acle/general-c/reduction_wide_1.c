/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svuint32x2_t u32x2)
{
  svaddv (pg); /* { dg-error {too few arguments to function 'svaddv'} } */
  svaddv (pg, u32, u32); /* { dg-error {too many arguments to function 'svaddv'} } */
  svaddv (0, u32); /* { dg-error {passing 'int' to argument 1 of 'svaddv', which expects 'svbool_t'} } */
  svaddv (u32, u32); /* { dg-error {passing 'svuint32_t' to argument 1 of 'svaddv', which expects 'svbool_t'} } */
  svaddv (pg, 0); /* { dg-error {passing 'int' to argument 2 of 'svaddv', which expects an SVE type rather than a scalar} } */
  svaddv (pg, pg); /* { dg-error {'svaddv' has no form that takes 'svbool_t' arguments} } */
  svaddv (pg, s32);
  svaddv (pg, u32);
  svaddv (pg, f32);
  svaddv (pg, u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 2 of 'svaddv', which expects a single SVE vector rather than a tuple} } */
}
