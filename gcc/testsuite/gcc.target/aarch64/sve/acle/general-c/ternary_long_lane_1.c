/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32, svfloat64_t f64, int i)
{
  svmlalb_lane (u64, u32, u32); /* { dg-error {too few arguments to function 'svmlalb_lane'} } */
  svmlalb_lane (u64, u32, u32, 0, 0); /* { dg-error {too many arguments to function 'svmlalb_lane'} } */
  svmlalb_lane (0, u16, u16, 0); /* { dg-error {passing 'int' to argument 1 of 'svmlalb_lane', which expects an SVE type rather than a scalar} } */
  svmlalb_lane (pg, u16, u16, 0); /* { dg-error {'svmlalb_lane' has no form that takes 'svbool_t' arguments} } */
  svmlalb_lane (u8, u8, u8, 0); /* { dg-error {'svmlalb_lane' has no form that takes 'svuint8_t' arguments} } */
  svmlalb_lane (u16, u8, u8, 0); /* { dg-error {'svmlalb_lane' has no form that takes 'svuint16_t' arguments} } */
  svmlalb_lane (f16, u16, u16, 0); /* { dg-error {'svmlalb_lane' has no form that takes 'svfloat16_t' arguments} } */
  svmlalb_lane (f32, f16, f16, 0);
  svmlalb_lane (u32, u16, u16, 0);
  svmlalb_lane (u32, 0, u16, 0); /* { dg-error {passing 'int' to argument 2 of 'svmlalb_lane', which expects an SVE type rather than a scalar} } */
  svmlalb_lane (u32, s16, u16, 0); /* { dg-error {arguments 1 and 2 of 'svmlalb_lane' must have the same signedness, but the values passed here have type 'svuint32_t' and 'svint16_t' respectively} } */
  svmlalb_lane (u32, u16, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svmlalb_lane', which expects an SVE type rather than a scalar} } */
  svmlalb_lane (u32, u16, s16, 0); /* { dg-error {arguments 1 and 3 of 'svmlalb_lane' must have the same signedness, but the values passed here have type 'svuint32_t' and 'svint16_t' respectively} } */
  svmlalb_lane (u32, u32, u32, 0); /* { dg-error {passing 'svuint32_t' instead of the expected 'svuint16_t' to argument 2 of 'svmlalb_lane', after passing 'svuint32_t' to argument 1} } */
  svmlalb_lane (u32, u8, u16, 0); /* { dg-error {passing 'svuint8_t' instead of the expected 'svuint16_t' to argument 2 of 'svmlalb_lane', after passing 'svuint32_t' to argument 1} } */
  svmlalb_lane (u32, u16, u8, 0); /* { dg-error {passing 'svuint8_t' instead of the expected 'svuint16_t' to argument 3 of 'svmlalb_lane', after passing 'svuint32_t' to argument 1} } */
  svmlalb_lane (u64, u32, u32, s32); /* { dg-error {argument 4 of 'svmlalb_lane' must be an integer constant expression} } */
  svmlalb_lane (u64, u32, u32, i); /* { dg-error {argument 4 of 'svmlalb_lane' must be an integer constant expression} } */

  svmlalb_lane (s32, s16, s16, 0);
  svmlalb_lane (s32, s16, s16, 7);
  svmlalb_lane (s32, s16, s16, 8); /* { dg-error {passing 8 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 7\]} } */
  svmlalb_lane (s32, s16, s16, -1); /* { dg-error {passing -1 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 7\]} } */

  svmlalb_lane (u32, u16, u16, 0);
  svmlalb_lane (u32, u16, u16, 7);
  svmlalb_lane (u32, u16, u16, 8); /* { dg-error {passing 8 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 7\]} } */
  svmlalb_lane (u32, u16, u16, -1); /* { dg-error {passing -1 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 7\]} } */

  svmlalb_lane (s64, s32, s32, 0);
  svmlalb_lane (s64, s32, s32, 3);
  svmlalb_lane (s64, s32, s32, 4); /* { dg-error {passing 4 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 3\]} } */
  svmlalb_lane (s64, s32, s32, -1); /* { dg-error {passing -1 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 3\]} } */

  svmlalb_lane (u64, u32, u32, 0);
  svmlalb_lane (u64, u32, u32, 3);
  svmlalb_lane (u64, u32, u32, 4); /* { dg-error {passing 4 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 3\]} } */
  svmlalb_lane (u64, u32, u32, -1); /* { dg-error {passing -1 to argument 4 of 'svmlalb_lane', which expects a value in the range \[0, 3\]} } */
}
