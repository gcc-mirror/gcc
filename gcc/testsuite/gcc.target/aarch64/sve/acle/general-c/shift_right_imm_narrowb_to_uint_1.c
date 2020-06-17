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
  svqshrunb (s16); /* { dg-error {too few arguments to function 'svqshrunb'} } */
  svqshrunb (s16, s16, 1); /* { dg-error {too many arguments to function 'svqshrunb'} } */

  svqshrunb (s16, x); /* { dg-error {argument 2 of 'svqshrunb' must be an integer constant expression} } */
  svqshrunb (s16, one); /* { dg-error {argument 2 of 'svqshrunb' must be an integer constant expression} } */
  svqshrunb (s16, 0.4); /* { dg-error {passing 0 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 8\]} } */
  svqshrunb (s16, 1.0);

  svqshrunb (pg, 1); /* { dg-error {'svqshrunb' has no form that takes 'svbool_t' arguments} } */

  svqshrunb (u8, -1); /* { dg-error {'svqshrunb' has no form that takes 'svuint8_t' arguments} } */
  svqshrunb (u8, 1); /* { dg-error {'svqshrunb' has no form that takes 'svuint8_t' arguments} } */
  svqshrunb (u8, 100); /* { dg-error {'svqshrunb' has no form that takes 'svuint8_t' arguments} } */

  svqshrunb (s8, 1); /* { dg-error {'svqshrunb' has no form that takes 'svint8_t' arguments} } */

  svqshrunb (u16, 1); /* { dg-error {'svqshrunb' has no form that takes 'svuint16_t' arguments} } */

  svqshrunb (s16, -1); /* { dg-error {passing -1 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 8\]} } */
  svqshrunb (s16, 0); /* { dg-error {passing 0 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 8\]} } */
  svqshrunb (s16, 1);
  svqshrunb (s16, 8);
  svqshrunb (s16, 9); /* { dg-error {passing 9 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 8\]} } */

  svqshrunb (u32, 1); /* { dg-error {'svqshrunb' has no form that takes 'svuint32_t' arguments} } */

  svqshrunb (s32, -1); /* { dg-error {passing -1 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 16\]} } */
  svqshrunb (s32, 0); /* { dg-error {passing 0 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 16\]} } */
  svqshrunb (s32, 1);
  svqshrunb (s32, 16);
  svqshrunb (s32, 17); /* { dg-error {passing 17 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 16\]} } */

  svqshrunb (u64, 1); /* { dg-error {'svqshrunb' has no form that takes 'svuint64_t' arguments} } */

  svqshrunb (s64, -1); /* { dg-error {passing -1 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 32\]} } */
  svqshrunb (s64, 0); /* { dg-error {passing 0 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 32\]} } */
  svqshrunb (s64, 1);
  svqshrunb (s64, 32);
  svqshrunb (s64, 33); /* { dg-error {passing 33 to argument 2 of 'svqshrunb', which expects a value in the range \[1, 32\]} } */

  svqshrunb (f32, 1); /* { dg-error {'svqshrunb' has no form that takes 'svfloat32_t' arguments} } */

  svqshrunb (1, 1); /* { dg-error {passing 'int' to argument 1 of 'svqshrunb', which expects an SVE vector type} } */
}
