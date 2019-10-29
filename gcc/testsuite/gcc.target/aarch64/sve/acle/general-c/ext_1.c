/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svfloat16_t f16, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, int i)
{
  svext (pg, pg, 0); /* { dg-error {'svext' has no form that takes 'svbool_t' arguments} } */
  svext (s8, s8, i); /* { dg-error {argument 3 of 'svext' must be an integer constant expression} } */

  svext (s8, s8, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 255\]} } */
  svext (s8, s8, 0);
  svext (s8, s8, 255);
  svext (s8, s8, 256); /* { dg-error {passing 256 to argument 3 of 'svext', which expects a value in the range \[0, 255\]} } */

  svext (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 255\]} } */
  svext (u8, u8, 0);
  svext (u8, u8, 255);
  svext (u8, u8, 256); /* { dg-error {passing 256 to argument 3 of 'svext', which expects a value in the range \[0, 255\]} } */

  svext (s16, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */
  svext (s16, s16, 0);
  svext (s16, s16, 127);
  svext (s16, s16, 128); /* { dg-error {passing 128 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */

  svext (u16, u16, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */
  svext (u16, u16, 0);
  svext (u16, u16, 127);
  svext (u16, u16, 128); /* { dg-error {passing 128 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */

  svext (f16, f16, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */
  svext (f16, f16, 0);
  svext (f16, f16, 127);
  svext (f16, f16, 128); /* { dg-error {passing 128 to argument 3 of 'svext', which expects a value in the range \[0, 127\]} } */

  svext (s32, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */
  svext (s32, s32, 0);
  svext (s32, s32, 63);
  svext (s32, s32, 64); /* { dg-error {passing 64 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */

  svext (u32, u32, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */
  svext (u32, u32, 0);
  svext (u32, u32, 63);
  svext (u32, u32, 64); /* { dg-error {passing 64 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */

  svext (f32, f32, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */
  svext (f32, f32, 0);
  svext (f32, f32, 63);
  svext (f32, f32, 64); /* { dg-error {passing 64 to argument 3 of 'svext', which expects a value in the range \[0, 63\]} } */

  svext (s64, s64, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */
  svext (s64, s64, 0);
  svext (s64, s64, 31);
  svext (s64, s64, 32); /* { dg-error {passing 32 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */

  svext (u64, u64, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */
  svext (u64, u64, 0);
  svext (u64, u64, 31);
  svext (u64, u64, 32); /* { dg-error {passing 32 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */

  svext (f64, f64, -1); /* { dg-error {passing -1 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */
  svext (f64, f64, 0);
  svext (f64, f64, 31);
  svext (f64, f64, 32); /* { dg-error {passing 32 to argument 3 of 'svext', which expects a value in the range \[0, 31\]} } */
}
