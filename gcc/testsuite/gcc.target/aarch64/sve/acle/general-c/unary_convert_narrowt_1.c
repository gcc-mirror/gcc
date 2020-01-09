#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, svfloat16_t f16, svfloat32_t f32,
      svfloat64_t f64)
{
  svcvtnt_f32_m (f32, pg); /* { dg-error {too few arguments to function 'svcvtnt_f32_m'} } */
  svcvtnt_f32_m (f32, pg, f64, 0); /* { dg-error {too many arguments to function 'svcvtnt_f32_m'} } */
  svcvtnt_f32_m (f16, pg, f64); /* { dg-error {passing 'svfloat16_t' to argument 1 of 'svcvtnt_f32_m', which expects 'svfloat32_t'} } */
  svcvtnt_f32_m (0, pg, f64); /* { dg-error {passing 'int' to argument 1 of 'svcvtnt_f32_m', which expects 'svfloat32_t'} } */
  svcvtnt_f32_m (pg, pg, f64); /* { dg-error {passing 'svbool_t' to argument 1 of 'svcvtnt_f32_m', which expects 'svfloat32_t'} } */
  svcvtnt_f32_m (f32, s32, f64); /* { dg-error {passing 'svint32_t' to argument 2 of 'svcvtnt_f32_m', which expects 'svbool_t'} } */
  svcvtnt_f32_m (f32, pg, 0); /* { dg-error {passing 'int' to argument 3 of 'svcvtnt_f32_m', which expects an SVE vector type} } */

  svcvtnt_f32_m (f32, pg, s8); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svint8_t' arguments} } */
  svcvtnt_f32_m (f32, pg, s16); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svint16_t' arguments} } */
  svcvtnt_f32_m (f32, pg, s32); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svint32_t' arguments} } */
  svcvtnt_f32_m (f32, pg, s64); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svint64_t' arguments} } */
  svcvtnt_f32_m (f32, pg, u8); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svuint8_t' arguments} } */
  svcvtnt_f32_m (f32, pg, u16); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svuint16_t' arguments} } */
  svcvtnt_f32_m (f32, pg, u32); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svuint32_t' arguments} } */
  svcvtnt_f32_m (f32, pg, u64); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svuint64_t' arguments} } */
  svcvtnt_f32_m (f32, pg, f16); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svfloat16_t' arguments} } */
  svcvtnt_f32_m (f32, pg, f32); /* { dg-error {'svcvtnt_f32_m' has no form that takes 'svfloat32_t' arguments} } */
  svcvtnt_f32_m (f32, pg, f64);
}
