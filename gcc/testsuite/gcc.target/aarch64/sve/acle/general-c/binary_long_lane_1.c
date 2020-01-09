/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32, svfloat64_t f64, int i)
{
  svmullb_lane (u32, u32); /* { dg-error {too few arguments to function 'svmullb_lane'} } */
  svmullb_lane (u32, u32, 0, 0); /* { dg-error {too many arguments to function 'svmullb_lane'} } */
  svmullb_lane (pg, pg, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svbool_t' arguments} } */
  svmullb_lane (s8, s8, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svint8_t' arguments} } */
  svmullb_lane (u8, u8, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svuint8_t' arguments} } */
  svmullb_lane (s64, s64, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svint64_t' arguments} } */
  svmullb_lane (u64, u64, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svuint64_t' arguments} } */
  svmullb_lane (f16, f16, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svfloat16_t' arguments} } */
  svmullb_lane (f32, f32, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svfloat32_t' arguments} } */
  svmullb_lane (f64, f64, 0); /* { dg-error {'svmullb_lane' has no form that takes 'svfloat64_t' arguments} } */
  svmullb_lane (1, u32, 0); /* { dg-error {passing 'int' to argument 1 of 'svmullb_lane', which expects an SVE vector type} } */
  svmullb_lane (u32, 1, 0); /* { dg-error {passing 'int' to argument 2 of 'svmullb_lane', which expects an SVE vector type} } */
  svmullb_lane (u32, s32, 0); /* { dg-error {passing 'svint32_t' to argument 2 of 'svmullb_lane', but previous arguments had type 'svuint32_t'} } */
  svmullb_lane (u32, u32, s32); /* { dg-error {argument 3 of 'svmullb_lane' must be an integer constant expression} } */
  svmullb_lane (u32, u32, i); /* { dg-error {argument 3 of 'svmullb_lane' must be an integer constant expression} } */

  svmullb_lane (s16, s16, 0);
  svmullb_lane (s16, s16, 7);
  svmullb_lane (s16, s16, 8); /* { dg-error {passing 8 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 7\]} } */
  svmullb_lane (s16, s16, -1); /* { dg-error {passing -1 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 7\]} } */

  svmullb_lane (u16, u16, 0);
  svmullb_lane (u16, u16, 7);
  svmullb_lane (u16, u16, 8); /* { dg-error {passing 8 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 7\]} } */
  svmullb_lane (u16, u16, -1); /* { dg-error {passing -1 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 7\]} } */

  svmullb_lane (s32, s32, 0);
  svmullb_lane (s32, s32, 3);
  svmullb_lane (s32, s32, 4); /* { dg-error {passing 4 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 3\]} } */
  svmullb_lane (s32, s32, -1); /* { dg-error {passing -1 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 3\]} } */

  svmullb_lane (u32, u32, 0);
  svmullb_lane (u32, u32, 3);
  svmullb_lane (u32, u32, 4); /* { dg-error {passing 4 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 3\]} } */
  svmullb_lane (u32, u32, -1); /* { dg-error {passing -1 to argument 3 of 'svmullb_lane', which expects a value in the range \[0, 3\]} } */
}
