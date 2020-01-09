/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat32_t f32, int i)
{
  svcdot_lane (u32, u8, u8, 0); /* { dg-error {too few arguments to function 'svcdot_lane'} } */
  svcdot_lane (u32, u8, u8, 0, 0, 0); /* { dg-error {too many arguments to function 'svcdot_lane'} } */
  svcdot_lane (0, u8, u8, 0, 0); /* { dg-error {passing 'int' to argument 1 of 'svcdot_lane', which expects an SVE vector type} } */
  svcdot_lane (pg, u8, u8, 0, 0); /* { dg-error {'svcdot_lane' has no form that takes 'svbool_t' arguments} } */
  svcdot_lane (s8, s8, s8, 0, 0); /* { dg-error {'svcdot_lane' has no form that takes 'svint8_t' arguments} } */
  svcdot_lane (f32, s8, s8, 0, 0); /* { dg-error {'svcdot_lane' has no form that takes 'svfloat32_t' arguments} } */
  svcdot_lane (s32, s8, s8, 0, 0);
  svcdot_lane (s32, 0, s8, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svcdot_lane', which expects an SVE vector type} } */
  svcdot_lane (s32, s8, 0, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svcdot_lane', which expects an SVE vector type} } */

  svcdot_lane (s32, s8, s8, 0, 0);
  svcdot_lane (s32, u8, s8, 0, 0); /* { dg-error {arguments 1 and 2 of 'svcdot_lane' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svcdot_lane (s32, s8, u8, 0, 0); /* { dg-error {arguments 1 and 3 of 'svcdot_lane' must have the same signedness, but the values passed here have type 'svint32_t' and 'svuint8_t' respectively} } */
  svcdot_lane (s32, s32, s8, 0, 0); /* { dg-error {passing 'svint32_t' instead of the expected 'svint8_t' to argument 2 of 'svcdot_lane', after passing 'svint32_t' to argument 1} } */
  svcdot_lane (s32, s8, s32, 0, 0); /* { dg-error {passing 'svint32_t' instead of the expected 'svint8_t' to argument 3 of 'svcdot_lane', after passing 'svint32_t' to argument 1} } */

  svcdot_lane (u32, u8, u8, 0, 0); /* { dg-error {'svcdot_lane' has no form that takes 'svuint32_t' arguments} } */

  svcdot_lane (s64, s16, s16, 0, 0);
  svcdot_lane (s64, u16, s16, 0, 0); /* { dg-error {arguments 1 and 2 of 'svcdot_lane' must have the same signedness, but the values passed here have type 'svint64_t' and 'svuint16_t' respectively} } */
  svcdot_lane (s64, s16, u16, 0, 0); /* { dg-error {arguments 1 and 3 of 'svcdot_lane' must have the same signedness, but the values passed here have type 'svint64_t' and 'svuint16_t' respectively} } */
  svcdot_lane (s64, s64, s16, 0, 0); /* { dg-error {passing 'svint64_t' instead of the expected 'svint16_t' to argument 2 of 'svcdot_lane', after passing 'svint64_t' to argument 1} } */
  svcdot_lane (s64, s16, s64, 0, 0); /* { dg-error {passing 'svint64_t' instead of the expected 'svint16_t' to argument 3 of 'svcdot_lane', after passing 'svint64_t' to argument 1} } */

  svcdot_lane (u64, u16, u16, 0, 0); /* { dg-error {'svcdot_lane' has no form that takes 'svuint64_t' arguments} } */

  svcdot_lane (s32, s8, s8, i, 0); /* { dg-error {argument 4 of 'svcdot_lane' must be an integer constant expression} } */
  svcdot_lane (s32, s8, s8, -1, 0); /* { dg-error {passing -1 to argument 4 of 'svcdot_lane', which expects a value in the range \[0, 3\]} } */
  svcdot_lane (s32, s8, s8, 0, 0);
  svcdot_lane (s32, s8, s8, 3, 0);
  svcdot_lane (s32, s8, s8, 4, 0); /* { dg-error {passing 4 to argument 4 of 'svcdot_lane', which expects a value in the range \[0, 3\]} } */

  svcdot_lane (s64, s16, s16, i, 0); /* { dg-error {argument 4 of 'svcdot_lane' must be an integer constant expression} } */
  svcdot_lane (s64, s16, s16, -1, 0); /* { dg-error {passing -1 to argument 4 of 'svcdot_lane', which expects a value in the range \[0, 1\]} } */
  svcdot_lane (s64, s16, s16, 0, 0);
  svcdot_lane (s64, s16, s16, 1, 0);
  svcdot_lane (s64, s16, s16, 2, 0); /* { dg-error {passing 2 to argument 4 of 'svcdot_lane', which expects a value in the range \[0, 1\]} } */

  svcdot_lane (s32, s8, s8, 0, i); /* { dg-error {argument 5 of 'svcdot_lane' must be an integer constant expression} } */
  svcdot_lane (s32, s8, s8, 0, -90); /* { dg-error {passing -90 to argument 5 of 'svcdot_lane', which expects 0, 90, 180 or 270} } */
  svcdot_lane (s32, s8, s8, 0, 0);
  svcdot_lane (s32, s8, s8, 0, 3); /* { dg-error {passing 3 to argument 5 of 'svcdot_lane', which expects 0, 90, 180 or 270} } */
  svcdot_lane (s32, s8, s8, 0, 90);
  svcdot_lane (s32, s8, s8, 0, 180);
  svcdot_lane (s32, s8, s8, 0, 270);
}
