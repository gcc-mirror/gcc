/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32)
{
  svaddwb (u16); /* { dg-error {too few arguments to function 'svaddwb'} } */
  svaddwb (u16, u8, u8); /* { dg-error {too many arguments to function 'svaddwb'} } */
  svaddwb (pg, pg); /* { dg-error {'svaddwb' has no form that takes 'svbool_t' arguments} } */
  svaddwb (u8, u8); /* { dg-error {'svaddwb' has no form that takes 'svuint8_t' arguments} } */
  svaddwb (s8, s8); /* { dg-error {'svaddwb' has no form that takes 'svint8_t' arguments} } */
  svaddwb (u16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 2 of 'svaddwb', after passing 'svuint16_t' to argument 1} } */
  svaddwb (u16, s16); /* { dg-error {passing 'svint16_t' instead of the expected 'svuint8_t' to argument 2 of 'svaddwb', after passing 'svuint16_t' to argument 1} } */
  svaddwb (u16, u8);
  svaddwb (u16, s8); /* { dg-error {arguments 1 and 2 of 'svaddwb' must have the same signedness, but the values passed here have type 'svuint16_t' and 'svint8_t' respectively} } */
  svaddwb (u16, pg); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 2 of 'svaddwb', after passing 'svuint16_t' to argument 1} } */
  svaddwb (s16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svint8_t' to argument 2 of 'svaddwb', after passing 'svint16_t' to argument 1} } */
  svaddwb (s16, s16); /* { dg-error {passing 'svint16_t' instead of the expected 'svint8_t' to argument 2 of 'svaddwb', after passing 'svint16_t' to argument 1} } */
  svaddwb (s16, u8); /* { dg-error {arguments 1 and 2 of 'svaddwb' must have the same signedness, but the values passed here have type 'svint16_t' and 'svuint8_t' respectively} } */
  svaddwb (s16, s8);
  svaddwb (f32, f16); /* { dg-error {'svaddwb' has no form that takes 'svfloat32_t' arguments} } */
  svaddwb (f16, f32); /* { dg-error {'svaddwb' has no form that takes 'svfloat16_t' arguments} } */
  svaddwb (0, u32); /* { dg-error {passing 'int' to argument 1 of 'svaddwb', which expects an SVE type rather than a scalar} } */
  svaddwb (0, u64); /* { dg-error {passing 'int' to argument 1 of 'svaddwb', which expects an SVE type rather than a scalar} } */
  svaddwb (u8, 0); /* { dg-error {'svaddwb' has no form that takes 'svuint8_t' arguments} } */
  svaddwb (u16, 0);
  svaddwb (u32, 0);
  svaddwb (u64, 0);
  svaddwb (pg, 0); /* { dg-error {'svaddwb' has no form that takes 'svbool_t' arguments} } */
  svaddwb (f32, 0); /* { dg-error {'svaddwb' has no form that takes 'svfloat32_t' arguments} } */
}
