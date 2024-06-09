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
  svqxtunt (u32); /* { dg-error {too few arguments to function 'svqxtunt'} } */
  svqxtunt (u32, s16, s16); /* { dg-error {too many arguments to function 'svqxtunt'} } */
  svqxtunt (u8, pg); /* { dg-error {'svqxtunt' has no form that takes 'svbool_t' arguments} } */
  svqxtunt (u8, u8); /* { dg-error {'svqxtunt' has no form that takes 'svuint8_t' arguments} } */
  svqxtunt (u8, s8); /* { dg-error {'svqxtunt' has no form that takes 'svint8_t' arguments} } */
  svqxtunt (u16, s16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svqxtunt', after passing 'svint16_t' to argument 2} } */
  svqxtunt (s8, s16); /* { dg-error {passing 'svint8_t' to argument 1 of 'svqxtunt', which expects a vector of unsigned integers} } */
  svqxtunt (pg, s16); /* { dg-error {passing 'svbool_t' to argument 1 of 'svqxtunt', which expects a vector of unsigned integers} } */
  svqxtunt (u8, u16); /* { dg-error {'svqxtunt' has no form that takes 'svuint16_t' arguments} } */
  svqxtunt (u8, s16);
  svqxtunt (u16, u32); /* { dg-error {'svqxtunt' has no form that takes 'svuint32_t' arguments} } */
  svqxtunt (u16, s32);
  svqxtunt (u32, u64); /* { dg-error {'svqxtunt' has no form that takes 'svuint64_t' arguments} } */
  svqxtunt (u32, s64);
  svqxtunt (u16, f32); /* { dg-error {'svqxtunt' has no form that takes 'svfloat32_t' arguments} } */
  svqxtunt (1, u16); /* { dg-error {passing 'int' to argument 1 of 'svqxtunt', which expects an SVE type rather than a scalar} } */
  svqxtunt (u8, 1); /* { dg-error {passing 'int' to argument 2 of 'svqxtunt', which expects an SVE type rather than a scalar} } */
}
