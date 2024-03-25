/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a+sve+i8mm" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svint64_t s64, svuint64_t u64,
    svfloat32_t f32, int i)
{
  svusdot_lane (s32, u8, s8); /* { dg-error {too few arguments to function 'svusdot_lane'} } */
  svusdot_lane (s32, u8, s8, 0, 0); /* { dg-error {too many arguments to function 'svusdot_lane'} } */
  svusdot_lane (0, u8, s8, 0); /* { dg-error {passing 'int' to argument 1 of 'svusdot_lane', which expects an SVE type rather than a scalar} } */
  svusdot_lane (pg, u8, s8, 0); /* { dg-error {'svusdot_lane' has no form that takes 'svbool_t' arguments} } */
  svusdot_lane (u8, u8, s8, 0); /* { dg-error {'svusdot_lane' has no form that takes 'svuint8_t' arguments} } */
  svusdot_lane (f32, u8, s8, 0); /* { dg-error {'svusdot_lane' has no form that takes 'svfloat32_t' arguments} } */
  svusdot_lane (u32, u8, s8, 0); /* { dg-error {'svusdot_lane' has no form that takes 'svuint32_t' arguments} } */
  svusdot_lane (s32, u8, s8, 0);
  svusdot_lane (s32, 0, s8, 0); /* { dg-error {passing 'int' to argument 2 of 'svusdot_lane', which expects an SVE type rather than a scalar} } */
  svusdot_lane (s32, u8, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svusdot_lane', which expects an SVE type rather than a scalar} } */

  svusdot_lane (s32, u8, s8, 0);
  svusdot_lane (s32, s8, s8, 0); /* { dg-error {passing 'svint8_t' to argument 2 of 'svusdot_lane', which expects a vector of unsigned integers} } */
  svusdot_lane (s32, u8, u8, 0); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusdot_lane', which expects a vector of signed integers} } */
  svusdot_lane (s32, s32, s32, 0); /* { dg-error {passing 'svint32_t' to argument 2 of 'svusdot_lane', which expects a vector of unsigned integers} } */

  svusdot_lane (s32, u8, s8, i); /* { dg-error {argument 4 of 'svusdot_lane' must be an integer constant expression} } */
  svusdot_lane (s32, u8, s8, 0);
  svusdot_lane (s32, u8, s8, 3);
  svusdot_lane (s32, u8, s8, 4);  /* { dg-error {passing 4 to argument 4 of 'svusdot_lane', which expects a value in the range \[0, 3\]} } */
  svusdot_lane (s32, u8, s8, -1);  /* { dg-error {passing -1 to argument 4 of 'svusdot_lane', which expects a value in the range \[0, 3\]} } */
}
