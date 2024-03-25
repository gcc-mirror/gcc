#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, svfloat16_t f16, svfloat32_t f32,
      svfloat64_t f64, svcount_t pn)
{
  svcvt_f64_x (pg); /* { dg-error {too few arguments to function 'svcvt_f64_x'} } */
  svcvt_f64_x (pg, s32, 0); /* { dg-error {too many arguments to function 'svcvt_f64_x'} } */
  svcvt_f64_x (s32, s32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svcvt_f64_x', which expects 'svbool_t'} } */
  svcvt_f64_x (pg, 0); /* { dg-error {passing 'int' to argument 2 of 'svcvt_f64_x', which expects an SVE type rather than a scalar} } */

  svcvt_f64_x (pg, s8); /* { dg-error {'svcvt_f64_x' has no form that takes 'svint8_t' arguments} } */
  svcvt_f64_x (pg, s16); /* { dg-error {'svcvt_f64_x' has no form that takes 'svint16_t' arguments} } */
  svcvt_f64_x (pg, s32);
  svcvt_f64_x (pg, s64);
  svcvt_f64_x (pg, u8); /* { dg-error {'svcvt_f64_x' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f64_x (pg, u16); /* { dg-error {'svcvt_f64_x' has no form that takes 'svuint16_t' arguments} } */
  svcvt_f64_x (pg, u32);
  svcvt_f64_x (pg, u64);
  svcvt_f64_x (pg, f16);
  svcvt_f64_x (pg, f32);
  svcvt_f64_x (pg, f64); /* { dg-error {'svcvt_f64_x' has no form that takes 'svfloat64_t' arguments} } */

  svcvt_f32_x (pg, s8); /* { dg-error {'svcvt_f32_x' has no form that takes 'svint8_t' arguments} } */
  svcvt_f32_x (pg, s16); /* { dg-error {'svcvt_f32_x' has no form that takes 'svint16_t' arguments} } */
  svcvt_f32_x (pg, s32);
  svcvt_f32_x (pg, s64);
  svcvt_f32_x (pg, u8); /* { dg-error {'svcvt_f32_x' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f32_x (pg, u16); /* { dg-error {'svcvt_f32_x' has no form that takes 'svuint16_t' arguments} } */
  svcvt_f32_x (pg, u32);
  svcvt_f32_x (pg, u64);
  svcvt_f32_x (pg, f16);
  svcvt_f32_x (pg, f32); /* { dg-error {'svcvt_f32_x' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_f32_x (pg, f64);

  svcvt_f16_x (pg, s8); /* { dg-error {'svcvt_f16_x' has no form that takes 'svint8_t' arguments} } */
  svcvt_f16_x (pg, s16);
  svcvt_f16_x (pg, s32);
  svcvt_f16_x (pg, s64);
  svcvt_f16_x (pg, u8); /* { dg-error {'svcvt_f16_x' has no form that takes 'svuint8_t' arguments} } */
  svcvt_f16_x (pg, u16);
  svcvt_f16_x (pg, u32);
  svcvt_f16_x (pg, u64);
  svcvt_f16_x (pg, f16); /* { dg-error {'svcvt_f16_x' has no form that takes 'svfloat16_t' arguments} } */
  svcvt_f16_x (pg, f32);
  svcvt_f16_x (pg, f64);

  svcvt_s64_x (pg, f16);
  svcvt_s64_x (pg, f32);
  svcvt_s64_x (pg, f64);

  svcvt_s32_x (pg, f16);
  svcvt_s32_x (pg, f32);
  svcvt_s32_x (pg, f64);

  svcvt_s16_x (pg, f16);
  svcvt_s16_x (pg, f32); /* { dg-error {'svcvt_s16_x' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_s16_x (pg, f64); /* { dg-error {'svcvt_s16_x' has no form that takes 'svfloat64_t' arguments} } */

  svcvt_u64_x (pg, f16);
  svcvt_u64_x (pg, f32);
  svcvt_u64_x (pg, f64);

  svcvt_u32_x (pg, f16);
  svcvt_u32_x (pg, f32);
  svcvt_u32_x (pg, f64);

  svcvt_u16_x (pg, f16);
  svcvt_u16_x (pg, f32); /* { dg-error {'svcvt_u16_x' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_u16_x (pg, f64); /* { dg-error {'svcvt_u16_x' has no form that takes 'svfloat64_t' arguments} } */

  svreinterpret_b (pg); /* { dg-error {'svreinterpret_b' has no form that takes 'svbool_t' arguments} } */
  svreinterpret_b (pn);

  svreinterpret_c (pg);
  svreinterpret_c (pn); /* { dg-error {'svreinterpret_c' has no form that takes 'svcount_t' arguments} } */
}
