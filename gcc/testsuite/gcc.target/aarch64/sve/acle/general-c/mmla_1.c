/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm+f32mm+f64mm" } */

#include <arm_sve.h>

svuint32_t
f1 (svint32_t s32, svuint8_t u8, svint8_t s8, svuint32_t u32)
{
  svmmla_s32 (s32); /* { dg-error {too few arguments to function 'svmmla_s32'} } */
  svmmla_s32 (s32, s8, s8, u32); /* { dg-error {too many arguments to function 'svmmla_s32'} } */
  svmmla_s32 (s32, u32, s8); /* { dg-error {incompatible type for argument 2 of 'svmmla_s32'} } */
  svmmla_s32 (s32, u8, s8); /* { dg-error {incompatible type for argument 2 of 'svmmla_s32'} } */
  svmmla_s32 (s32, s8, u8); /* { dg-error {incompatible type for argument 3 of 'svmmla_s32'} } */
  svmmla_s32 (s32, s8, s32); /* { dg-error {incompatible type for argument 3 of 'svmmla_s32'} } */
  svmmla_s32 (s32, s8, 0); /* { dg-error {incompatible type for argument 3 of 'svmmla_s32'} } */
  svmmla_s32 (s32, s8, s8);
  return svmmla_s32 (s32, s8, s8); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint32_t u32, svint32_t s32,
    svfloat16_t f16, svfloat32_t f32, svfloat64_t f64)
{
  svmmla (s32, s8); /* { dg-error {too few arguments to function 'svmmla'} } */
  svmmla (s32, s8, s8, s8); /* { dg-error {too many arguments to function 'svmmla'} } */
  svmmla (0, s8, s8); /* { dg-error {passing 'int' to argument 1 of 'svmmla', which expects an SVE vector type} } */
  svmmla (pg, s8, s8); /* { dg-error {'svmmla' has no form that takes 'svbool_t' arguments} } */
  svmmla (u8, s8, s8); /* { dg-error {'svmmla' has no form that takes 'svuint8_t' arguments} } */

  svmmla (s32, 0, s8); /* { dg-error {passing 'int' to argument 2 of 'svmmla', which expects an SVE vector type} } */
  svmmla (s32, u8, s8); /* { dg-error {arguments 1 and 2 of 'svmmla' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svmmla (s32, s8, u8); /* { dg-error {arguments 1 and 3 of 'svmmla' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svmmla (s32, s8, 0); /* { dg-error {passing 'int' to argument 3 of 'svmmla', which expects an SVE vector type} } */
  svmmla (s32, s8, s8);
  svmmla (s32, s32, s32); /* { dg-error {passing 'svint32_t' instead of the expected 'svint8_t' to argument 2 of 'svmmla', after passing 'svint32_t' to argument 1} } */
  svmmla (s32, u32, u32); /* { dg-error {passing 'svuint32_t' instead of the expected 'svint8_t' to argument 2 of 'svmmla', after passing 'svint32_t' to argument 1} } */

  svmmla (u32, 0, u8); /* { dg-error {passing 'int' to argument 2 of 'svmmla', which expects an SVE vector type} } */
  svmmla (u32, s8, u8); /* { dg-error {arguments 1 and 2 of 'svmmla' must have the same signedness, but the values passed here have type 'svuint32_t' and 'svint8_t' respectively} } */
  svmmla (u32, u8, s8); /* { dg-error {arguments 1 and 3 of 'svmmla' must have the same signedness, but the values passed here have type 'svuint32_t' and 'svint8_t' respectively} } */
  svmmla (u32, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svmmla', which expects an SVE vector type} } */
  svmmla (u32, u8, u8);
  svmmla (u32, s32, s32); /* { dg-error {passing 'svint32_t' instead of the expected 'svuint8_t' to argument 2 of 'svmmla', after passing 'svuint32_t' to argument 1} } */
  svmmla (u32, u32, u32); /* { dg-error {passing 'svuint32_t' instead of the expected 'svuint8_t' to argument 2 of 'svmmla', after passing 'svuint32_t' to argument 1} } */

  svmmla (f16, s8, s8); /* { dg-error {'svmmla' has no form that takes 'svfloat16_t' arguments} } */
  svmmla (f32, s8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svmmla', but previous arguments had type 'svfloat32_t'} } */
  svmmla (f32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svmmla', but previous arguments had type 'svfloat32_t'} } */
  svmmla (f32, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svmmla', but previous arguments had type 'svfloat32_t'} } */
  svmmla (f64, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svmmla', but previous arguments had type 'svfloat64_t'} } */
  svmmla (f32, f32, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmmla', but previous arguments had type 'svfloat32_t'} } */
  svmmla (f64, f32, f16); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svmmla', but previous arguments had type 'svfloat64_t'} } */
  svmmla (f64, f64, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmmla', but previous arguments had type 'svfloat64_t'} } */

  svmmla (f16, f16, f16); /* { dg-error {'svmmla' has no form that takes 'svfloat16_t' arguments} } */
  svmmla (f32, f32, f32);
  svmmla (f64, f64, f64);
}
