/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64,
    svfloat32_t f32, int x)
{
  const int one = 1;
  svqshrunt (u8, s16); /* { dg-error {too few arguments to function 'svqshrunt'} } */
  svqshrunt (u8, s16, s16, 1); /* { dg-error {too many arguments to function 'svqshrunt'} } */

  svqshrunt (u16, s16, 1); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svqshrunt', after passing 'svint16_t' to argument 2} } */
  svqshrunt (s16, s16, 1); /* { dg-error {passing 'svint16_t' to argument 1 of 'svqshrunt', which expects a vector of unsigned integers} } */
  svqshrunt (s8, s16, 1); /* { dg-error {passing 'svint8_t' to argument 1 of 'svqshrunt', which expects a vector of unsigned integers} } */
  svqshrunt (pg, s16, 1); /* { dg-error {passing 'svbool_t' to argument 1 of 'svqshrunt', which expects a vector of unsigned integers} } */

  svqshrunt (u8, s16, x); /* { dg-error {argument 3 of 'svqshrunt' must be an integer constant expression} } */
  svqshrunt (u8, s16, one); /* { dg-error {argument 3 of 'svqshrunt' must be an integer constant expression} } */
  svqshrunt (u8, s16, 0.4); /* { dg-error {passing 0 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 8\]} } */
  svqshrunt (u8, s16, 1.0);

  svqshrunt (u8, pg, 1); /* { dg-error {'svqshrunt' has no form that takes 'svbool_t' arguments} } */

  svqshrunt (u8, u8, -1); /* { dg-error {'svqshrunt' has no form that takes 'svuint8_t' arguments} } */
  svqshrunt (u8, u8, 1); /* { dg-error {'svqshrunt' has no form that takes 'svuint8_t' arguments} } */
  svqshrunt (u8, u8, 100); /* { dg-error {'svqshrunt' has no form that takes 'svuint8_t' arguments} } */

  svqshrunt (u8, s8, 1); /* { dg-error {'svqshrunt' has no form that takes 'svint8_t' arguments} } */

  svqshrunt (u8, u16, 1); /* { dg-error {'svqshrunt' has no form that takes 'svuint16_t' arguments} } */

  svqshrunt (u8, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 8\]} } */
  svqshrunt (u8, s16, 0); /* { dg-error {passing 0 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 8\]} } */
  svqshrunt (u8, s16, 1);
  svqshrunt (u8, s16, 8);
  svqshrunt (u8, s16, 9); /* { dg-error {passing 9 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 8\]} } */

  svqshrunt (u16, u32, 1); /* { dg-error {'svqshrunt' has no form that takes 'svuint32_t' arguments} } */

  svqshrunt (u16, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 16\]} } */
  svqshrunt (u16, s32, 0); /* { dg-error {passing 0 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 16\]} } */
  svqshrunt (u16, s32, 1);
  svqshrunt (u16, s32, 16);
  svqshrunt (u16, s32, 17); /* { dg-error {passing 17 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 16\]} } */

  svqshrunt (u32, u64, 1); /* { dg-error {'svqshrunt' has no form that takes 'svuint64_t' arguments} } */

  svqshrunt (u32, s64, -1); /* { dg-error {passing -1 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 32\]} } */
  svqshrunt (u32, s64, 0); /* { dg-error {passing 0 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 32\]} } */
  svqshrunt (u32, s64, 1);
  svqshrunt (u32, s64, 32);
  svqshrunt (u32, s64, 33); /* { dg-error {passing 33 to argument 3 of 'svqshrunt', which expects a value in the range \[1, 32\]} } */

  svqshrunt (u16, f32, 1); /* { dg-error {'svqshrunt' has no form that takes 'svfloat32_t' arguments} } */

  svqshrunt (1, u32, 1); /* { dg-error {passing 'int' to argument 1 of 'svqshrunt', which expects an SVE type rather than a scalar} } */
  svqshrunt (u32, 1, 1); /* { dg-error {passing 'int' to argument 2 of 'svqshrunt', which expects an SVE type rather than a scalar} } */
}
