/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a+sve+i8mm" } */

#include <arm_sve.h>

svuint32_t
f1 (svint32_t s32, svuint8_t u8, svint8_t s8, svuint32_t u32)
{
  svusmmla_s32 (s32); /* { dg-error {too few arguments to function 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u8, s8, u32); /* { dg-error {too many arguments to function 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u32, s8); /* { dg-error {incompatible type for argument 2 of 'svusmmla_s32'} } */
  svusmmla_s32 (s32, s8, s8); /* { dg-error {incompatible type for argument 2 of 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u8, s32); /* { dg-error {incompatible type for argument 3 of 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svusmmla_s32'} } */
  svusmmla_s32 (s32, u8, s8);
  return svusmmla_s32 (s32, u8, s8); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint32_t u32,
    svint32_t s32, svfloat32_t f32)
{
  svusmmla (s32, u8); /* { dg-error {too few arguments to function 'svusmmla'} } */
  svusmmla (s32, u8, s8, u8); /* { dg-error {too many arguments to function 'svusmmla'} } */
  svusmmla (0, u8, s8); /* { dg-error {passing 'int' to argument 1 of 'svusmmla', which expects an SVE vector type} } */
  svusmmla (pg, u8, s8); /* { dg-error {'svusmmla' has no form that takes 'svbool_t' arguments} } */
  svusmmla (u8, u8, s8); /* { dg-error {'svusmmla' has no form that takes 'svuint8_t' arguments} } */
  svusmmla (f32, u8, s8); /* { dg-error {'svusmmla' has no form that takes 'svfloat32_t' arguments} } */
  svusmmla (s32, u8, s8);
  svusmmla (s32, 0, s8); /* { dg-error {passing 'int' to argument 2 of 'svusmmla', which expects an SVE vector type} } */
  svusmmla (s32, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusmmla', which expects a vector of signed integers} } */
  svusmmla (s32, s8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svusmmla', which expects a vector of unsigned integers} } */
  svusmmla (s32, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svusmmla', which expects an SVE vector type} } */
  svusmmla (s32, u8, s8);
  svusmmla (s32, u32, u32); /* { dg-error {passing 'svuint32_t' instead of the expected 'svuint8_t' to argument 2 of 'svusmmla', after passing 'svint32_t' to argument 1} } */
}
