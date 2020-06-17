/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svint16_t s16, svint32_t s32,
    svint64_t s64, int x)
{
  const int one = 1;
  svuint16_t u16 __attribute__((unused));
  svuint32_t u32 __attribute__((unused));
  svuint64_t u64 __attribute__((unused));
  u8 = svqshlu_x (pg, u8, 1); /* { dg-error {'svqshlu_x' has no form that takes 'svuint8_t' arguments} } */
  u8 = svqshlu_x (pg, s8, x); /* { dg-error {argument 3 of 'svqshlu_x' must be an integer constant expression} } */
  u8 = svqshlu_x (pg, s8, one); /* { dg-error {argument 3 of 'svqshlu_x' must be an integer constant expression} } */
  u8 = svqshlu_x (pg, s8, -1); /* { dg-error {passing -1 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 7\]} } */
  u8 = svqshlu_x (pg, s8, 0.0);
  u8 = svqshlu_x (pg, s8, 0);
  u8 = svqshlu_x (pg, s8, 1);
  u8 = svqshlu_x (pg, s8, 1 + 1);
  u8 = svqshlu_x (pg, s8, 7);
  u8 = svqshlu_x (pg, s8, 7.2);
  u8 = svqshlu_x (pg, s8, 8); /* { dg-error {passing 8 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 7\]} } */
  u8 = svqshlu_x (pg, s8, 8.2); /* { dg-error {passing 8 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 7\]} } */
  u8 = svqshlu_x (pg, s8, (1ULL << 62) + 1); /* { dg-error {passing [^ ]* to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 7\]} } */
  u16 = svqshlu_x (pg, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 15\]} } */
  u16 = svqshlu_x (pg, s16, 0);
  u16 = svqshlu_x (pg, s16, 1);
  u16 = svqshlu_x (pg, s16, 15);
  u16 = svqshlu_x (pg, s16, 16); /* { dg-error {passing 16 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 15\]} } */
  u32 = svqshlu_x (pg, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 31\]} } */
  u32 = svqshlu_x (pg, s32, 0);
  u32 = svqshlu_x (pg, s32, 1);
  u32 = svqshlu_x (pg, s32, 31);
  u32 = svqshlu_x (pg, s32, 32); /* { dg-error {passing 32 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 31\]} } */
  u64 = svqshlu_x (pg, s64, -1); /* { dg-error {passing -1 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 63\]} } */
  u64 = svqshlu_x (pg, s64, 0);
  u64 = svqshlu_x (pg, s64, 1);
  u64 = svqshlu_x (pg, s64, 63);
  u64 = svqshlu_x (pg, s64, 64); /* { dg-error {passing 64 to argument 3 of 'svqshlu_x', which expects a value in the range \[0, 63\]} } */
}
