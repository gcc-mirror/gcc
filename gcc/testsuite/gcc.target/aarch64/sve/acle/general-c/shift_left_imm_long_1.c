/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32, svfloat64_t f64, int x)
{
  const int one = 1;
  s16 = svshllb (s8, x); /* { dg-error {argument 2 of 'svshllb' must be an integer constant expression} } */
  s16 = svshllb (s8, one); /* { dg-error {argument 2 of 'svshllb' must be an integer constant expression} } */
  s16 = svshllb (s8, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  s16 = svshllb (s8, 0.0);
  s16 = svshllb (s8, 0);
  s16 = svshllb (s8, 1);
  s16 = svshllb (s8, 1 + 1);
  s16 = svshllb (s8, 7);
  s16 = svshllb (s8, 7.2);
  s16 = svshllb (s8, 8); /* { dg-error {passing 8 to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  s16 = svshllb (s8, 8.2); /* { dg-error {passing 8 to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  s16 = svshllb (s8, (1ULL << 62) + 1); /* { dg-error {passing [^ ]* to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  u16 = svshllb (u8, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  u16 = svshllb (u8, 0);
  u16 = svshllb (u8, 1);
  u16 = svshllb (u8, 7);
  u16 = svshllb (u8, 8); /* { dg-error {passing 8 to argument 2 of 'svshllb', which expects a value in the range \[0, 7\]} } */
  s32 = svshllb (s16, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 15\]} } */
  s32 = svshllb (s16, 0);
  s32 = svshllb (s16, 1);
  s32 = svshllb (s16, 15);
  s32 = svshllb (s16, 16); /* { dg-error {passing 16 to argument 2 of 'svshllb', which expects a value in the range \[0, 15\]} } */
  u32 = svshllb (u16, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 15\]} } */
  u32 = svshllb (u16, 0);
  u32 = svshllb (u16, 1);
  u32 = svshllb (u16, 15);
  u32 = svshllb (u16, 16); /* { dg-error {passing 16 to argument 2 of 'svshllb', which expects a value in the range \[0, 15\]} } */
  s64 = svshllb (s32, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 31\]} } */
  s64 = svshllb (s32, 0);
  s64 = svshllb (s32, 1);
  s64 = svshllb (s32, 31);
  s64 = svshllb (s32, 32); /* { dg-error {passing 32 to argument 2 of 'svshllb', which expects a value in the range \[0, 31\]} } */
  u64 = svshllb (u32, -1); /* { dg-error {passing -1 to argument 2 of 'svshllb', which expects a value in the range \[0, 31\]} } */
  u64 = svshllb (u32, 0);
  u64 = svshllb (u32, 1);
  u64 = svshllb (u32, 31);
  u64 = svshllb (u32, 32); /* { dg-error {passing 32 to argument 2 of 'svshllb', which expects a value in the range \[0, 31\]} } */
  svshllb (s64, -1); /* { dg-error {'svshllb' has no form that takes 'svint64_t' arguments} } */
  svshllb (u64, -1); /* { dg-error {'svshllb' has no form that takes 'svuint64_t' arguments} } */
  svshllb (pg, -1); /* { dg-error {'svshllb' has no form that takes 'svbool_t' arguments} } */
  svshllb (f16, -1); /* { dg-error {'svshllb' has no form that takes 'svfloat16_t' arguments} } */
  svshllb (f32, -1); /* { dg-error {'svshllb' has no form that takes 'svfloat32_t' arguments} } */
  svshllb (f64, -1); /* { dg-error {'svshllb' has no form that takes 'svfloat64_t' arguments} } */
}
