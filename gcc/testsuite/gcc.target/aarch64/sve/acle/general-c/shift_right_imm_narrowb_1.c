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
  svshrnb (u16); /* { dg-error {too few arguments to function 'svshrnb'} } */
  svshrnb (u16, u16, 1); /* { dg-error {too many arguments to function 'svshrnb'} } */

  svshrnb (u16, x); /* { dg-error {argument 2 of 'svshrnb' must be an integer constant expression} } */
  svshrnb (u16, one); /* { dg-error {argument 2 of 'svshrnb' must be an integer constant expression} } */
  svshrnb (u16, 0.4); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */
  svshrnb (u16, 1.0);

  svshrnb (pg, 1); /* { dg-error {'svshrnb' has no form that takes 'svbool_t' arguments} } */

  svshrnb (u8, -1); /* { dg-error {'svshrnb' has no form that takes 'svuint8_t' arguments} } */
  svshrnb (u8, 1); /* { dg-error {'svshrnb' has no form that takes 'svuint8_t' arguments} } */
  svshrnb (u8, 100); /* { dg-error {'svshrnb' has no form that takes 'svuint8_t' arguments} } */

  svshrnb (s8, 1); /* { dg-error {'svshrnb' has no form that takes 'svint8_t' arguments} } */

  svshrnb (u16, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */
  svshrnb (u16, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */
  svshrnb (u16, 1);
  svshrnb (u16, 8);
  svshrnb (u16, 9); /* { dg-error {passing 9 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */

  svshrnb (s16, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */
  svshrnb (s16, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */
  svshrnb (s16, 1);
  svshrnb (s16, 8);
  svshrnb (s16, 9); /* { dg-error {passing 9 to argument 2 of 'svshrnb', which expects a value in the range \[1, 8\]} } */

  svshrnb (u32, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */
  svshrnb (u32, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */
  svshrnb (u32, 1);
  svshrnb (u32, 16);
  svshrnb (u32, 17); /* { dg-error {passing 17 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */

  svshrnb (s32, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */
  svshrnb (s32, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */
  svshrnb (s32, 1);
  svshrnb (s32, 16);
  svshrnb (s32, 17); /* { dg-error {passing 17 to argument 2 of 'svshrnb', which expects a value in the range \[1, 16\]} } */

  svshrnb (u64, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */
  svshrnb (u64, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */
  svshrnb (u64, 1);
  svshrnb (u64, 32);
  svshrnb (u64, 33); /* { dg-error {passing 33 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */

  svshrnb (s64, -1); /* { dg-error {passing -1 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */
  svshrnb (s64, 0); /* { dg-error {passing 0 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */
  svshrnb (s64, 1);
  svshrnb (s64, 32);
  svshrnb (s64, 33); /* { dg-error {passing 33 to argument 2 of 'svshrnb', which expects a value in the range \[1, 32\]} } */

  svshrnb (f32, 1); /* { dg-error {'svshrnb' has no form that takes 'svfloat32_t' arguments} } */

  svshrnb (1, 1); /* { dg-error {passing 'int' to argument 1 of 'svshrnb', which expects an SVE type rather than a scalar} } */
}
