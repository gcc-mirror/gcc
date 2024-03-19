/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a+sve+i8mm" } */

#include <arm_sve.h>

svuint32_t
f1 (svint32_t s32, svuint8_t u8, svint8_t s8, svuint32_t u32)
{
  svusdot_s32 (s32); /* { dg-error {too few arguments to function 'svusdot_s32'} } */
  svusdot_s32 (s32, u8, s8, u32); /* { dg-error {too many arguments to function 'svusdot_s32'} } */
  svusdot_s32 (s32, u32, s8); /* { dg-error {incompatible type for argument 2 of 'svusdot_s32'} } */
  svusdot_s32 (s32, s8, s8); /* { dg-error {incompatible type for argument 2 of 'svusdot_s32'} } */
  svusdot_s32 (s32, u8, u8); /* { dg-error {incompatible type for argument 3 of 'svusdot_s32'} } */
  svusdot_s32 (s32, u8, s32); /* { dg-error {incompatible type for argument 3 of 'svusdot_s32'} } */
  svusdot_s32 (s32, u8, 0); /* { dg-error {incompatible type for argument 3 of 'svusdot_s32'} } */
  svusdot_s32 (s32, u8, s8);
  return svusdot_s32 (s32, u8, s8); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint32_t u32,
    svint32_t s32, svfloat32_t f32)
{
  svusdot (s32, u8); /* { dg-error {too few arguments to function 'svusdot'} } */
  svusdot (s32, u8, s8, u8); /* { dg-error {too many arguments to function 'svusdot'} } */
  svusdot (0, u8, s8); /* { dg-error {passing 'int' to argument 1 of 'svusdot', which expects an SVE type rather than a scalar} } */
  svusdot (pg, u8, s8); /* { dg-error {'svusdot' has no form that takes 'svbool_t' arguments} } */
  svusdot (u8, u8, s8); /* { dg-error {'svusdot' has no form that takes 'svuint8_t' arguments} } */
  svusdot (f32, u8, s8); /* { dg-error {'svusdot' has no form that takes 'svfloat32_t' arguments} } */
  svusdot (s32, u8, s8);
  svusdot (s32, 0, s8); /* { dg-error {passing 'int' to argument 2 of 'svusdot', which expects an SVE type rather than a scalar} } */
  svusdot (s32, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusdot', which expects a vector of signed integers} } */
  svusdot (s32, s8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svusdot', which expects a vector of unsigned integers} } */
  svusdot (s32, u8, 0);
  svusdot (s32, u8, s8);
  svusdot (s32, u32, u32); /* { dg-error {passing 'svuint32_t' instead of the expected 'svuint8_t' to argument 2 of 'svusdot', after passing 'svint32_t' to argument 1} } */
}
