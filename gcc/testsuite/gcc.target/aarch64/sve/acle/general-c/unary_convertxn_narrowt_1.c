#include <arm_sve.h>

#pragma GCC target "+sme2+fp8"

void
test (svmfloat8_t f8, svfloat32x2_t f32x2, fpm_t fpm0,
      svfloat16x2_t f16x2, svfloat16x4_t f16x4,
      svfloat32x3_t f32x3, svfloat32x4_t f32x4,
      svbool_t pg, float f, svint8_t s8, svint32x2_t s32x2)
  __arm_streaming
{
  svcvtnt_mf8_fpm (f8, f32x2, fpm0);

  svcvtnt_mf8_fpm (); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  
  svcvtnt_mf8_fpm (f8); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (f32x2); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (fpm0); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (f); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (f8, f32x2); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (f32x2, fpm0); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (f8, fpm0); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (pg); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */
  svcvtnt_mf8_fpm (s8); /* { dg-error {too few arguments to function 'svcvtnt_mf8_fpm'} } */

  svcvtnt_mf8_fpm (f8, f16x2, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svfloat16x2_t' arguments} } */
  svcvtnt_mf8_fpm (f8, f16x4, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svfloat16x4_t' arguments} } */
  svcvtnt_mf8_fpm (f8, f32x3, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svfloat32x3_t' arguments} } */
  svcvtnt_mf8_fpm (f8, f32x4, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svfloat32x4_t' arguments} } */

  svcvtnt_mf8_fpm (f8, 0, fpm0); /* { dg-error {passing 'int' to argument 2 of 'svcvtnt_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtnt_mf8_fpm (f8, f, fpm0); /* { dg-error {passing 'float' to argument 2 of 'svcvtnt_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtnt_mf8_fpm (f8, pg, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svbool_t' arguments} } */
  svcvtnt_mf8_fpm (f8, s8, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svint8_t' arguments} } */
  svcvtnt_mf8_fpm (f8, s32x2, fpm0); /* { dg-error {'svcvtnt_mf8_fpm' has no form that takes 'svint32x2_t' arguments} } */
  
  svcvtnt_mf8_fpm (f8, f32x2, f32x2); /* { dg-error {passing 'svfloat32x2_t' to argument 3 of 'svcvtnt_mf8_fpm', which expects 'uint64_t'} } */
}
