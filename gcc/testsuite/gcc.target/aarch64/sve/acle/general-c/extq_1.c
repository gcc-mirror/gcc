/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svfloat16_t f16, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, int i)
{
  svextq (pg, pg, 0); /* { dg-error {'svextq' has no form that takes 'svbool_t' arguments} } */
  svextq (s8, s8, i); /* { dg-error {argument 3 of 'svextq' must be an integer constant expression} } */

  svextq (s8, s8, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 15\]} } */
  svextq (s8, s8, 0);
  svextq (s8, s8, 15);
  svextq (s8, s8, 16); /* { dg-error {passing 16 to argument 3 of 'svextq', which expects a value in the range \[0, 15\]} } */

  svextq (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 15\]} } */
  svextq (u8, u8, 0);
  svextq (u8, u8, 15);
  svextq (u8, u8, 16); /* { dg-error {passing 16 to argument 3 of 'svextq', which expects a value in the range \[0, 15\]} } */

  svextq (s16, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */
  svextq (s16, s16, 0);
  svextq (s16, s16, 7);
  svextq (s16, s16, 8); /* { dg-error {passing 8 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */

  svextq (u16, u16, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */
  svextq (u16, u16, 0);
  svextq (u16, u16, 7);
  svextq (u16, u16, 8); /* { dg-error {passing 8 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */

  svextq (f16, f16, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */
  svextq (f16, f16, 0);
  svextq (f16, f16, 7);
  svextq (f16, f16, 8); /* { dg-error {passing 8 to argument 3 of 'svextq', which expects a value in the range \[0, 7\]} } */

  svextq (s32, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */
  svextq (s32, s32, 0);
  svextq (s32, s32, 3);
  svextq (s32, s32, 4); /* { dg-error {passing 4 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */

  svextq (u32, u32, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */
  svextq (u32, u32, 0);
  svextq (u32, u32, 3);
  svextq (u32, u32, 4); /* { dg-error {passing 4 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */

  svextq (f32, f32, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */
  svextq (f32, f32, 0);
  svextq (f32, f32, 3);
  svextq (f32, f32, 4); /* { dg-error {passing 4 to argument 3 of 'svextq', which expects a value in the range \[0, 3\]} } */

  svextq (s64, s64, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */
  svextq (s64, s64, 0);
  svextq (s64, s64, 1);
  svextq (s64, s64, 2); /* { dg-error {passing 2 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */

  svextq (u64, u64, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */
  svextq (u64, u64, 0);
  svextq (u64, u64, 1);
  svextq (u64, u64, 2); /* { dg-error {passing 2 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */

  svextq (f64, f64, -1); /* { dg-error {passing -1 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */
  svextq (f64, f64, 0);
  svextq (f64, f64, 1);
  svextq (f64, f64, 2); /* { dg-error {passing 2 to argument 3 of 'svextq', which expects a value in the range \[0, 1\]} } */
}

#pragma GCC target "+nosve2p1"

void
f2 (svint8_t s8)
{
  svextq (s8, s8, 0); /* { dg-error {ACLE function 'svextq_s8' requires ISA extension 'sve2p1'} } */
}
