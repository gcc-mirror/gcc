#include <arm_sve.h>

#pragma GCC target "+sme2+fp8"

void
test (svfloat16x2_t f16x2, svbfloat16x2_t bf16x2, svfloat32x2_t f32x2,
      svfloat16x3_t f16x3, svfloat16x4_t f16x4,
      svfloat32x3_t f32x3, svfloat32x4_t f32x4,
      fpm_t fpm0,
      svbool_t pg, float f, svint8_t s8, svint32x2_t s32x2)
  __arm_streaming
{
  svcvtn_mf8_fpm (f16x2, fpm0);
  svcvtn_mf8_fpm (bf16x2, fpm0);

  svcvtn_mf8_fpm (); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */
  
  svcvtn_mf8_fpm (f16x2); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */
  svcvtn_mf8_fpm (fpm0); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */
  
  svcvtn_mf8_fpm (f); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */
  svcvtn_mf8_fpm (pg); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */
  svcvtn_mf8_fpm (s8); /* { dg-error {too few arguments to function 'svcvtn_mf8_fpm'} } */

  svcvtn_mf8_fpm (f16x2, f16x2, fpm0); /* { dg-error {too many arguments to function 'svcvtn_mf8_fpm'} } */

  svcvtn_mf8_fpm (f16x3, fpm0); /* { dg-error {'svcvtn_mf8_fpm' has no form that takes 'svfloat16x3_t' arguments} } */
  svcvtn_mf8_fpm (f16x4, fpm0); /* { dg-error {'svcvtn_mf8_fpm' has no form that takes 'svfloat16x4_t' arguments} } */
  svcvtn_mf8_fpm (0, fpm0); /* { dg-error {passing 'int' to argument 1 of 'svcvtn_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtn_mf8_fpm (f, fpm0); /* { dg-error {passing 'float' to argument 1 of 'svcvtn_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtn_mf8_fpm (pg, fpm0); /* { dg-error {'svcvtn_mf8_fpm' has no form that takes 'svbool_t' arguments} } */
  svcvtn_mf8_fpm (s8, fpm0); /* { dg-error {'svcvtn_mf8_fpm' has no form that takes 'svint8_t' arguments} } */
  svcvtn_mf8_fpm (s32x2, fpm0); /* { dg-error {'svcvtn_mf8_fpm' has no form that takes 'svint32x2_t' arguments} } */
  
  svcvtn_mf8_fpm (f16x2, f16x2); /* { dg-error {passing 'svfloat16x2_t' to argument 2 of 'svcvtn_mf8_fpm', which expects 'uint64_t'} } */


  svcvtnb_mf8_fpm (f32x2, fpm0);

  svcvtnb_mf8_fpm (); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */
  
  svcvtnb_mf8_fpm (f32x2); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */
  svcvtnb_mf8_fpm (fpm0); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */
  
  svcvtnb_mf8_fpm (f); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */
  svcvtnb_mf8_fpm (pg); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */
  svcvtnb_mf8_fpm (s8); /* { dg-error {too few arguments to function 'svcvtnb_mf8_fpm'} } */

  svcvtnb_mf8_fpm (f32x2, f32x2, fpm0); /* { dg-error {too many arguments to function 'svcvtnb_mf8_fpm'} } */

  svcvtnb_mf8_fpm (f32x3, fpm0); /* { dg-error {'svcvtnb_mf8_fpm' has no form that takes 'svfloat32x3_t' arguments} } */
  svcvtnb_mf8_fpm (f32x4, fpm0); /* { dg-error {'svcvtnb_mf8_fpm' has no form that takes 'svfloat32x4_t' arguments} } */
  svcvtnb_mf8_fpm (0, fpm0); /* { dg-error {passing 'int' to argument 1 of 'svcvtnb_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtnb_mf8_fpm (f, fpm0); /* { dg-error {passing 'float' to argument 1 of 'svcvtnb_mf8_fpm', which expects an SVE type rather than a scalar type} } */
  svcvtnb_mf8_fpm (pg, fpm0); /* { dg-error {'svcvtnb_mf8_fpm' has no form that takes 'svbool_t' arguments} } */
  svcvtnb_mf8_fpm (s8, fpm0); /* { dg-error {'svcvtnb_mf8_fpm' has no form that takes 'svint8_t' arguments} } */
  svcvtnb_mf8_fpm (s32x2, fpm0); /* { dg-error {'svcvtnb_mf8_fpm' has no form that takes 'svint32x2_t' arguments} } */
  
  svcvtnb_mf8_fpm (f32x2, f32x2); /* { dg-error {passing 'svfloat32x2_t' to argument 2 of 'svcvtnb_mf8_fpm', which expects 'uint64_t'} } */
}
