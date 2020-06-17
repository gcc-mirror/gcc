/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svint16_t s16,
    svint32_t s32, svint64_t s64, int x)
{
  const int one = 1;
  pg = svsra (pg, pg, 1); /* { dg-error {'svsra' has no form that takes 'svbool_t' arguments} } */
  pg = svsra (pg, s8, 1); /* { dg-error {passing 'svint8_t' to argument 2 of 'svsra', but previous arguments had type 'svbool_t'} } */
  s8 = svsra (1, s8, 1); /* { dg-error {passing 'int' to argument 1 of 'svsra', which expects an SVE vector type} } */
  s8 = svsra (s8, u8, 1); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svsra', but previous arguments had type 'svint8_t'} } */
  s8 = svsra (s8, pg, 1); /* { dg-error {passing 'svbool_t' to argument 2 of 'svsra', but previous arguments had type 'svint8_t'} } */
  s8 = svsra (s8, 1, 1); /* { dg-error {passing 'int' to argument 2 of 'svsra', which expects an SVE vector type} } */
  s8 = svsra (s8, s8, x); /* { dg-error {argument 3 of 'svsra' must be an integer constant expression} } */
  s8 = svsra (s8, s8, one); /* { dg-error {argument 3 of 'svsra' must be an integer constant expression} } */
  s8 = svsra (s8, s8, 0.4); /* { dg-error {passing 0 to argument 3 of 'svsra', which expects a value in the range \[1, 8\]} } */
  s8 = svsra (s8, s8, 1.0);
  s8 = svsra (s8, s8, 0); /* { dg-error {passing 0 to argument 3 of 'svsra', which expects a value in the range \[1, 8\]} } */
  s8 = svsra (s8, s8, 1);
  s8 = svsra (s8, s8, 1 + 1);
  s8 = svsra (s8, s8, 8);
  s8 = svsra (s8, s8, 9); /* { dg-error {passing 9 to argument 3 of 'svsra', which expects a value in the range \[1, 8\]} } */
  s8 = svsra (s8, s8, (1ULL << 62) + 1); /* { dg-error {passing [^ ]* to argument 3 of 'svsra', which expects a value in the range \[1, 8\]} } */
  s16 = svsra (s16, s16, 0); /* { dg-error {passing 0 to argument 3 of 'svsra', which expects a value in the range \[1, 16\]} } */
  s16 = svsra (s16, s16, 1);
  s16 = svsra (s16, s16, 16);
  s16 = svsra (s16, s16, 17); /* { dg-error {passing 17 to argument 3 of 'svsra', which expects a value in the range \[1, 16\]} } */
  s32 = svsra (s32, s32, 0); /* { dg-error {passing 0 to argument 3 of 'svsra', which expects a value in the range \[1, 32\]} } */
  s32 = svsra (s32, s32, 1);
  s32 = svsra (s32, s32, 32);
  s32 = svsra (s32, s32, 33); /* { dg-error {passing 33 to argument 3 of 'svsra', which expects a value in the range \[1, 32\]} } */
  s64 = svsra (s64, s64, 0); /* { dg-error {passing 0 to argument 3 of 'svsra', which expects a value in the range \[1, 64\]} } */
  s64 = svsra (s64, s64, 1);
  s64 = svsra (s64, s64, 64);
  s64 = svsra (s64, s64, 65); /* { dg-error {passing 65 to argument 3 of 'svsra', which expects a value in the range \[1, 64\]} } */
}
