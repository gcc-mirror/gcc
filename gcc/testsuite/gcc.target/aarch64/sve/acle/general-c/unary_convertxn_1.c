#include <arm_sve.h>

#pragma GCC target "+sme2"

void
test (svbool_t pg, float f, svint8_t s8, svfloat32_t f32,
      svint32x2_t s32x2, svint32x3_t s32x3, svint32x4_t s32x4,
      svfloat32x2_t f32x2, svfloat32x3_t f32x3, svfloat32x4_t f32x4)
  __arm_streaming
{
  svcvt_bf16 (); /* { dg-error {too few arguments to function 'svcvt_bf16'} } */
  svcvt_bf16 (f32x2, f32x2); /* { dg-error {too many arguments to function 'svcvt_bf16'} } */
  svcvt_bf16 (0); /* { dg-error {passing 'int' to argument 1 of 'svcvt_bf16', which expects an SVE type rather than a scalar} } */
  svcvt_bf16 (f); /* { dg-error {passing 'float' to argument 1 of 'svcvt_bf16', which expects an SVE type rather than a scalar} } */
  svcvt_bf16 (pg); /* { dg-error {svcvt_bf16' has no form that takes 'svbool_t' arguments} } */
  svcvt_bf16 (s8); /* { dg-error {svcvt_bf16' has no form that takes 'svint8_t' arguments} } */
  svcvt_bf16 (f32); /* { dg-error {svcvt_bf16' has no form that takes 'svfloat32_t' arguments} } */
  svcvt_bf16 (f32x2);
  svcvt_bf16 (f32x3); /* { dg-error {svcvt_bf16' has no form that takes 'svfloat32x3_t' arguments} } */
  svcvt_bf16 (f32x4); /* { dg-error {svcvt_bf16' has no form that takes 'svfloat32x4_t' arguments} } */
  svcvt_bf16 (s32x2); /* { dg-error {svcvt_bf16' has no form that takes 'svint32x2_t' arguments} } */
  svcvt_s32 (f32x2);
  svcvt_s32 (f32x3); /* { dg-error {svcvt_s32' has no form that takes 'svfloat32x3_t' arguments} } */
  svcvt_s32 (f32x4);
  svcvt_f32 (s32x2);
  svcvt_f32 (s32x3); /* { dg-error {svcvt_f32' has no form that takes 'svint32x3_t' arguments} } */
  svcvt_f32 (s32x4);
}
