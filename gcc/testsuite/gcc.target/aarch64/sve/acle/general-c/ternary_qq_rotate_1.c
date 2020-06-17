/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat32_t f32, int i)
{
  svcdot (u32, u8, u8); /* { dg-error {too few arguments to function 'svcdot'} } */
  svcdot (u32, u8, u8, 0, 0); /* { dg-error {too many arguments to function 'svcdot'} } */
  svcdot (0, u8, u8, 0); /* { dg-error {passing 'int' to argument 1 of 'svcdot', which expects an SVE vector type} } */
  svcdot (pg, u8, u8, 0); /* { dg-error {'svcdot' has no form that takes 'svbool_t' arguments} } */
  svcdot (s8, s8, s8, 0); /* { dg-error {'svcdot' has no form that takes 'svint8_t' arguments} } */
  svcdot (f32, s8, s8, 0); /* { dg-error {'svcdot' has no form that takes 'svfloat32_t' arguments} } */
  svcdot (s32, s8, s8, 0);
  svcdot (s32, 0, s8, 0); /* { dg-error {passing 'int' to argument 2 of 'svcdot', which expects an SVE vector type} } */
  svcdot (s32, s8, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svcdot', which expects an SVE vector type} } */

  svcdot (s32, s8, s8, 0);
  svcdot (s32, u8, s8, 0); /* { dg-error {arguments 1 and 2 of 'svcdot' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svcdot (s32, s8, u8, 0); /* { dg-error {arguments 1 and 3 of 'svcdot' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svcdot (s32, s32, s8, 0); /* { dg-error {passing 'svint32_t' instead of the expected 'svint8_t' to argument 2 of 'svcdot', after passing 'svint32_t' to argument 1} } */
  svcdot (s32, s8, s32, 0); /* { dg-error {passing 'svint32_t' instead of the expected 'svint8_t' to argument 3 of 'svcdot', after passing 'svint32_t' to argument 1} } */

  svcdot (u32, u8, u8, 0); /* { dg-error {'svcdot' has no form that takes 'svuint32_t' arguments} } */

  svcdot (s64, s16, s16, 0);
  svcdot (s64, u16, s16, 0); /* { dg-error {arguments 1 and 2 of 'svcdot' must have the same signedness, but the values passed here have type 'svint64_t' and 'svuint16_t' respectively} } */
  svcdot (s64, s16, u16, 0); /* { dg-error {arguments 1 and 3 of 'svcdot' must have the same signedness, but the values passed here have type 'svint64_t' and 'svuint16_t' respectively} } */
  svcdot (s64, s64, s16, 0); /* { dg-error {passing 'svint64_t' instead of the expected 'svint16_t' to argument 2 of 'svcdot', after passing 'svint64_t' to argument 1} } */
  svcdot (s64, s16, s64, 0); /* { dg-error {passing 'svint64_t' instead of the expected 'svint16_t' to argument 3 of 'svcdot', after passing 'svint64_t' to argument 1} } */

  svcdot (u64, u16, u16, 0); /* { dg-error {'svcdot' has no form that takes 'svuint64_t' arguments} } */

  svcdot (s32, s8, s8, i); /* { dg-error {argument 4 of 'svcdot' must be an integer constant expression} } */
  svcdot (s32, s8, s8, -90); /* { dg-error {passing -90 to argument 4 of 'svcdot', which expects 0, 90, 180 or 270} } */
  svcdot (s32, s8, s8, 0);
  svcdot (s32, s8, s8, 3); /* { dg-error {passing 3 to argument 4 of 'svcdot', which expects 0, 90, 180 or 270} } */
  svcdot (s32, s8, s8, 90);
  svcdot (s32, s8, s8, 180);
  svcdot (s32, s8, s8, 270);
}
