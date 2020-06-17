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
  svqxtnt (u32); /* { dg-error {too few arguments to function 'svqxtnt'} } */
  svqxtnt (u32, u16, u16); /* { dg-error {too many arguments to function 'svqxtnt'} } */
  svqxtnt (pg, pg); /* { dg-error {'svqxtnt' has no form that takes 'svbool_t' arguments} } */
  svqxtnt (u8, u8); /* { dg-error {'svqxtnt' has no form that takes 'svuint8_t' arguments} } */
  svqxtnt (s8, s8); /* { dg-error {'svqxtnt' has no form that takes 'svint8_t' arguments} } */
  svqxtnt (u16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svqxtnt', after passing 'svuint16_t' to argument 2} } */
  svqxtnt (s8, u16); /* { dg-error {arguments 1 and 2 of 'svqxtnt' must have the same signedness, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svqxtnt (pg, u16); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 1 of 'svqxtnt', after passing 'svuint16_t' to argument 2} } */
  svqxtnt (u8, u16);
  svqxtnt (s8, s16);
  svqxtnt (u16, u32);
  svqxtnt (s16, s32);
  svqxtnt (u32, u64);
  svqxtnt (s32, s64);
  svqxtnt (f16, f32); /* { dg-error {'svqxtnt' has no form that takes 'svfloat32_t' arguments} } */
  svqxtnt (1, u16); /* { dg-error {passing 'int' to argument 1 of 'svqxtnt', which expects an SVE vector type} } */
  svqxtnt (u8, 1); /* { dg-error {passing 'int' to argument 2 of 'svqxtnt', which expects an SVE vector type} } */
}
