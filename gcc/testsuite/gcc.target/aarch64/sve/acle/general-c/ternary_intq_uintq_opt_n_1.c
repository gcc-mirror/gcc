/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a+sve+i8mm" } */

#include <arm_sve.h>

svuint32_t
f1 (svint32_t s32, svuint8_t u8, svint8_t s8, svuint32_t u32)
{
  svsudot_s32 (s32); /* { dg-error {too few arguments to function 'svsudot_s32'} } */
  svsudot_s32 (s32, s8, u8, u32); /* { dg-error {too many arguments to function 'svsudot_s32'} } */
  svsudot_s32 (s32, s32, u8); /* { dg-error {incompatible type for argument 2 of 'svsudot_s32'} } */
  svsudot_s32 (s32, u8, u8); /* { dg-error {incompatible type for argument 2 of 'svsudot_s32'} } */
  svsudot_s32 (s32, s8, u32); /* { dg-error {incompatible type for argument 3 of 'svsudot_s32'} } */
  svsudot_s32 (s32, s8, s8); /* { dg-error {incompatible type for argument 3 of 'svsudot_s32'} } */
  svsudot_s32 (s32, s8, 0); /* { dg-error {incompatible type for argument 3 of 'svsudot_s32'} } */
  svsudot_s32 (s32, s8, u8);
  return svsudot_s32 (s32, s8, u8); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint32_t u32,
    svint32_t s32, svfloat32_t f32)
{
  svsudot (s32, s8); /* { dg-error {too few arguments to function 'svsudot'} } */
  svsudot (s32, s8, u8, u8); /* { dg-error {too many arguments to function 'svsudot'} } */
  svsudot (0, s8, u8); /* { dg-error {passing 'int' to argument 1 of 'svsudot', which expects an SVE type rather than a scalar} } */
  svsudot (pg, s8, u8); /* { dg-error {'svsudot' has no form that takes 'svbool_t' arguments} } */
  svsudot (u8, s8, u8); /* { dg-error {'svsudot' has no form that takes 'svuint8_t' arguments} } */
  svsudot (f32, s8, u8); /* { dg-error {'svsudot' has no form that takes 'svfloat32_t' arguments} } */
  svsudot (s32, s8, u8);
  svsudot (s32, 0, u8); /* { dg-error {passing 'int' to argument 2 of 'svsudot', which expects an SVE type rather than a scalar} } */
  svsudot (s32, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svsudot', which expects a vector of signed integers} } */
  svsudot (s32, s8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svsudot', which expects a vector of unsigned integers} } */
  svsudot (s32, s8, 0);
  svsudot (s32, s8, u8);
  svsudot (s32, u32, u32); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svsudot', which expects a vector of signed integers} } */
}
