#include <arm_sve.h>

void
f (svint8_t s8, svuint16_t u16, svfloat32_t f32,
   svint16x2_t s16x2, svuint32x3_t u32x3, svfloat64x4_t f64x4,
   svbool_t pg)
{
  s8 = svlsr_x (pg, s8, 1); /* { dg-error {'svlsr_x' has no form that takes 'svint8_t' arguments} } */
  u16 = svneg_x (pg, u16); /* { dg-error {'svneg_x' has no form that takes 'svuint16_t' arguments} } */
  f32 = svclz_x (pg, f32); /* { dg-error {'svclz_x' has no form that takes 'svfloat32_t' arguments} } */
  s16x2 = svcreate2 (s8); /* { dg-error {too few arguments to function 'svcreate2'} } */
  u32x3 = svcreate3 (u16, u16, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svcreate3', but argument 1 had type 'svuint16_t'} } */
  f64x4 = svcreate4 (f32, f32, f32, f32, f32); /* { dg-error {too many arguments to function 'svcreate4'} } */
  pg = svadd_x (pg, pg, pg); /* { dg-error {'svadd_x' has no form that takes 'svbool_t' arguments} } */
}
