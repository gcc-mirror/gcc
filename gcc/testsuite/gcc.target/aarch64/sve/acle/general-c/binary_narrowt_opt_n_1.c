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
  svaddhnt (u32, u16); /* { dg-error {too few arguments to function 'svaddhnt'} } */
  svaddhnt (u32, u16, u16, u16); /* { dg-error {too many arguments to function 'svaddhnt'} } */
  svaddhnt (pg, pg, pg); /* { dg-error {'svaddhnt' has no form that takes 'svbool_t' arguments} } */
  svaddhnt (u8, u8, u8); /* { dg-error {'svaddhnt' has no form that takes 'svuint8_t' arguments} } */
  svaddhnt (s8, s8, s8); /* { dg-error {'svaddhnt' has no form that takes 'svint8_t' arguments} } */
  svaddhnt (u16, u16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svaddhnt', after passing 'svuint16_t' to argument 2} } */
  svaddhnt (s8, u16, u16); /* { dg-error {arguments 1 and 2 of 'svaddhnt' must have the same signedness, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svaddhnt (pg, u16, u16); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 1 of 'svaddhnt', after passing 'svuint16_t' to argument 2} } */
  svaddhnt (u8, u16, u16);
  svaddhnt (s8, s16, s16);
  svaddhnt (u16, u32, u32);
  svaddhnt (s16, s32, s32);
  svaddhnt (u32, u64, u64);
  svaddhnt (s32, s64, s64);
  svaddhnt (f16, f32, f32); /* { dg-error {'svaddhnt' has no form that takes 'svfloat32_t' arguments} } */
  svaddhnt (1, u16, u16); /* { dg-error {passing 'int' to argument 1 of 'svaddhnt', which expects an SVE vector type} } */
  svaddhnt (u8, 1, u16); /* { dg-error {passing 'int' to argument 2 of 'svaddhnt', which expects an SVE vector type} } */
  svaddhnt (u8, u16, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svaddhnt', but previous arguments had type 'svuint16_t'} } */
  svaddhnt (u8, u16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svaddhnt', but previous arguments had type 'svuint16_t'} } */
  svaddhnt (u8, u16, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svaddhnt', but previous arguments had type 'svuint16_t'} } */
  svaddhnt (u8, u16, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svaddhnt', but previous arguments had type 'svuint16_t'} } */
  svaddhnt (u8, u8, 0); /* { dg-error {'svaddhnt' has no form that takes 'svuint8_t' arguments} } */
  svaddhnt (u16, u16, 0); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svaddhnt', after passing 'svuint16_t' to argument 2} } */
  svaddhnt (s8, u16, 0); /* { dg-error {arguments 1 and 2 of 'svaddhnt' must have the same signedness, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svaddhnt (pg, u16, 0); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 1 of 'svaddhnt', after passing 'svuint16_t' to argument 2} } */
  svaddhnt (u8, u16, 0);
  svaddhnt (u16, u32, 0);
  svaddhnt (u32, u64, 0);
  svaddhnt (pg, pg, 0); /* { dg-error {'svaddhnt' has no form that takes 'svbool_t' arguments} } */
}
