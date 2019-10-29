#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, svfloat16_t f16, svfloat32_t f32,
      svfloat64_t f64)
{
  svcvt_f64_m (f64, pg); /* { dg-error {too few arguments to function 'svcvt_f64_m'} } */
  svcvt_f64_m (f64, pg, s32, 0); /* { dg-error {too many arguments to function 'svcvt_f64_m'} } */
  svcvt_f64_m (f32, pg, s32); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svcvt_f64_m', which expects 'svfloat64_t'} } */
  svcvt_f64_m (0, pg, s32); /* { dg-error {passing 'int' to argument 1 of 'svcvt_f64_m', which expects 'svfloat64_t'} } */
  svcvt_f64_m (pg, pg, s32); /* { dg-error {passing 'svbool_t' to argument 1 of 'svcvt_f64_m', which expects 'svfloat64_t'} } */
  svcvt_f64_m (f64, s32, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svcvt_f64_m', which expects 'svbool_t'} } */
  svcvt_f64_m (f64, pg, 0); /* { dg-error {passing 'int' to argument 3 of 'svcvt_f64_m', which expects an SVE vector type} } */

  svcvt_f64_m (f64, pg, s8); /* { dg-error {'svcvt_f64_m' has no form that takes 'svint8_t' arguments} } */
  svcvt_f64_m (f64, pg, s16); /* { dg-error {'svcvt_f64_m' has no form that takes 'svint16_t' arguments} } */
  svcvt_f64_m (f64, pg, s32);
  svcvt_f64_m (f64, pg, s64);
  svcvt_f64_m (f64, pg, u8); /* { dg-error {'svcvt_f64_m' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f64_m (f64, pg, u16); /* { dg-error {'svcvt_f64_m' has no form that takes 'svuint16_t' arguments} } */
  svcvt_f64_m (f64, pg, u32);
  svcvt_f64_m (f64, pg, u64);
  svcvt_f64_m (f64, pg, f16);
  svcvt_f64_m (f64, pg, f32);
  svcvt_f64_m (f64, pg, f64); /* { dg-error {'svcvt_f64_m' has no form that takes 'svfloat64_t' arguments} } */

  svcvt_f32_m (f32, pg, s8); /* { dg-error {'svcvt_f32_m' has no form that takes 'svint8_t' arguments} } */
  svcvt_f32_m (f32, pg, s16); /* { dg-error {'svcvt_f32_m' has no form that takes 'svint16_t' arguments} } */
  svcvt_f32_m (f32, pg, s32);
  svcvt_f32_m (f32, pg, s64);
  svcvt_f32_m (f32, pg, u8); /* { dg-error {'svcvt_f32_m' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f32_m (f32, pg, u16); /* { dg-error {'svcvt_f32_m' has no form that takes 'svuint16_t' arguments} } */
  svcvt_f32_m (f32, pg, u32);
  svcvt_f32_m (f32, pg, u64);
  svcvt_f32_m (f32, pg, f16);
  svcvt_f32_m (f32, pg, f32); /* { dg-error {'svcvt_f32_m' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_f32_m (f32, pg, f64);

  svcvt_f16_m (f16, pg, s8); /* { dg-error {'svcvt_f16_m' has no form that takes 'svint8_t' arguments} } */
  svcvt_f16_m (f16, pg, s16);
  svcvt_f16_m (f16, pg, s32);
  svcvt_f16_m (f16, pg, s64);
  svcvt_f16_m (f16, pg, u8); /* { dg-error {'svcvt_f16_m' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f16_m (f16, pg, u16);
  svcvt_f16_m (f16, pg, u32);
  svcvt_f16_m (f16, pg, u64);
  svcvt_f16_m (f16, pg, f16); /* { dg-error {'svcvt_f16_m' has no form that takes 'svfloat16_t' arguments} } */
  svcvt_f16_m (f16, pg, f32);
  svcvt_f16_m (f16, pg, f64);

  svcvt_s64_m (s64, pg, f16);
  svcvt_s64_m (s64, pg, f32);
  svcvt_s64_m (s64, pg, f64);

  svcvt_s32_m (s32, pg, f16);
  svcvt_s32_m (s32, pg, f32);
  svcvt_s32_m (s32, pg, f64);

  svcvt_s16_m (s16, pg, f16);
  svcvt_s16_m (s16, pg, f32); /* { dg-error {'svcvt_s16_m' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_s16_m (s16, pg, f64); /* { dg-error {'svcvt_s16_m' has no form that takes 'svfloat64_t' arguments} } */

  svcvt_u64_m (u64, pg, f16);
  svcvt_u64_m (u64, pg, f32);
  svcvt_u64_m (u64, pg, f64);

  svcvt_u32_m (u32, pg, f16);
  svcvt_u32_m (u32, pg, f32);
  svcvt_u32_m (u32, pg, f64);

  svcvt_u16_m (u16, pg, f16);
  svcvt_u16_m (u16, pg, f32); /* { dg-error {'svcvt_u16_m' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_u16_m (u16, pg, f64); /* { dg-error {'svcvt_u16_m' has no form that takes 'svfloat64_t' arguments} } */
}
