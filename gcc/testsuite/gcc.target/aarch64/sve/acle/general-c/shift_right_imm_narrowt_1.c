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
  svshrnt (u8, u16); /* { dg-error {too few arguments to function 'svshrnt'} } */
  svshrnt (u8, u16, u16, 1); /* { dg-error {too many arguments to function 'svshrnt'} } */

  svshrnt (u16, u16, 1); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svshrnt', after passing 'svuint16_t' to argument 2} } */
  svshrnt (s16, u16, 1); /* { dg-error {passing 'svint16_t' instead of the expected 'svuint8_t' to argument 1 of 'svshrnt', after passing 'svuint16_t' to argument 2} } */
  svshrnt (s8, u16, 1); /* { dg-error {arguments 1 and 2 of 'svshrnt' must have the same signedness, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svshrnt (pg, u16, 1); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 1 of 'svshrnt', after passing 'svuint16_t' to argument 2} } */

  svshrnt (s16, s16, 1); /* { dg-error {passing 'svint16_t' instead of the expected 'svint8_t' to argument 1 of 'svshrnt', after passing 'svint16_t' to argument 2} } */
  svshrnt (u16, s16, 1); /* { dg-error {passing 'svuint16_t' instead of the expected 'svint8_t' to argument 1 of 'svshrnt', after passing 'svint16_t' to argument 2} } */
  svshrnt (u8, s16, 1); /* { dg-error {arguments 1 and 2 of 'svshrnt' must have the same signedness, but the values passed here have type 'svuint8_t' and 'svint16_t' respectively} } */
  svshrnt (pg, s16, 1); /* { dg-error {passing 'svbool_t' instead of the expected 'svint8_t' to argument 1 of 'svshrnt', after passing 'svint16_t' to argument 2} } */

  svshrnt (u8, u16, x); /* { dg-error {argument 3 of 'svshrnt' must be an integer constant expression} } */
  svshrnt (u8, u16, one); /* { dg-error {argument 3 of 'svshrnt' must be an integer constant expression} } */
  svshrnt (u8, u16, 0.4); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */
  svshrnt (u8, u16, 1.0);

  svshrnt (pg, pg, 1); /* { dg-error {'svshrnt' has no form that takes 'svbool_t' arguments} } */

  svshrnt (u8, u8, -1); /* { dg-error {'svshrnt' has no form that takes 'svuint8_t' arguments} } */
  svshrnt (u8, u8, 1); /* { dg-error {'svshrnt' has no form that takes 'svuint8_t' arguments} } */
  svshrnt (u8, u8, 100); /* { dg-error {'svshrnt' has no form that takes 'svuint8_t' arguments} } */

  svshrnt (s8, s8, 1); /* { dg-error {'svshrnt' has no form that takes 'svint8_t' arguments} } */

  svshrnt (u8, u16, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */
  svshrnt (u8, u16, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */
  svshrnt (u8, u16, 1);
  svshrnt (u8, u16, 8);
  svshrnt (u8, u16, 9); /* { dg-error {passing 9 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */

  svshrnt (s8, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */
  svshrnt (s8, s16, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */
  svshrnt (s8, s16, 1);
  svshrnt (s8, s16, 8);
  svshrnt (s8, s16, 9); /* { dg-error {passing 9 to argument 3 of 'svshrnt', which expects a value in the range \[1, 8\]} } */

  svshrnt (u16, u32, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */
  svshrnt (u16, u32, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */
  svshrnt (u16, u32, 1);
  svshrnt (u16, u32, 16);
  svshrnt (u16, u32, 17); /* { dg-error {passing 17 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */

  svshrnt (s16, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */
  svshrnt (s16, s32, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */
  svshrnt (s16, s32, 1);
  svshrnt (s16, s32, 16);
  svshrnt (s16, s32, 17); /* { dg-error {passing 17 to argument 3 of 'svshrnt', which expects a value in the range \[1, 16\]} } */

  svshrnt (u32, u64, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */
  svshrnt (u32, u64, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */
  svshrnt (u32, u64, 1);
  svshrnt (u32, u64, 32);
  svshrnt (u32, u64, 33); /* { dg-error {passing 33 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */

  svshrnt (s32, s64, -1); /* { dg-error {passing -1 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */
  svshrnt (s32, s64, 0); /* { dg-error {passing 0 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */
  svshrnt (s32, s64, 1);
  svshrnt (s32, s64, 32);
  svshrnt (s32, s64, 33); /* { dg-error {passing 33 to argument 3 of 'svshrnt', which expects a value in the range \[1, 32\]} } */

  svshrnt (f32, f32, 1); /* { dg-error {'svshrnt' has no form that takes 'svfloat32_t' arguments} } */

  svshrnt (1, s32, 1); /* { dg-error {passing 'int' to argument 1 of 'svshrnt', which expects an SVE vector type} } */
  svshrnt (s32, 1, 1); /* { dg-error {passing 'int' to argument 2 of 'svshrnt', which expects an SVE vector type} } */
}
