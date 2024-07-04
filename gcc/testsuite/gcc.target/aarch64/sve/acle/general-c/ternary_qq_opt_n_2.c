/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint32_t u32,
    svfloat32_t f32)
{
  svdot (u32, u8); /* { dg-error {too few arguments to function 'svdot'} } */
  svdot (u32, u8, u8, u8); /* { dg-error {too many arguments to function 'svdot'} } */
  svdot (0, u8, u8); /* { dg-error {passing 'int' to argument 1 of 'svdot', which expects an SVE type rather than a scalar} } */
  svdot (pg, u8, u8); /* { dg-error {'svdot' has no form that takes 'svbool_t' and 'svuint8_t' arguments} }*/
  svdot (u8, u8, u8); /* { dg-error {'svdot' has no form that takes 'svuint8_t' and 'svuint8_t' arguments} } */
  svdot (f32, u8, u8); /* { dg-error {'svdot' has no form that takes 'svfloat32_t' and 'svuint8_t' arguments} } */
  svdot (u32, u8, u8);
  svdot (u32, 0, u8); /* { dg-error {passing 'int' to argument 2 of 'svdot', which expects an SVE type rather than a scalar} } */
  svdot (u32, s8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svdot', but argument 2 had type 'svint8_t'} } */
  svdot (u32, u8, 0);
  svdot (u32, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svdot', but argument 2 had type 'svuint8_t'} } */
  svdot (u32, u32, u32); /* { dg-error {'svdot' has no form that takes 'svuint32_t' and 'svuint32_t' arguments} } */
}
