/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve+bf16")

void
f1 (svbool_t pg, svuint8_t u8, svuint16_t u16, svint32_t s32,
    svbfloat16_t bf16, svfloat32_t f32, svfloat64_t f64, int i)
{
  svbfmlalb_lane (f32, bf16, bf16); /* { dg-error {too few arguments to function 'svbfmlalb_lane'} } */
  svbfmlalb_lane (f32, bf16, bf16, 0, 0); /* { dg-error {too many arguments to function 'svbfmlalb_lane'} } */
  svbfmlalb_lane (0, bf16, bf16, 0); /* { dg-error {passing 'int' to argument 1 of 'svbfmlalb_lane', which expects an SVE vector type} } */
  svbfmlalb_lane (pg, bf16, bf16, 0); /* { dg-error {'svbfmlalb_lane' has no form that takes 'svbool_t' arguments} } */
  svbfmlalb_lane (u8, bf16, bf16, 0); /* { dg-error {'svbfmlalb_lane' has no form that takes 'svuint8_t' arguments} } */
  svbfmlalb_lane (u16, bf16, bf16, 0); /* { dg-error {'svbfmlalb_lane' has no form that takes 'svuint16_t' arguments} } */
  svbfmlalb_lane (f64, bf16, bf16, 0); /* { dg-error {'svbfmlalb_lane' has no form that takes 'svfloat64_t' arguments} } */
  svbfmlalb_lane (f32, bf16, bf16, 0);
  svbfmlalb_lane (f32, 0, bf16, 0); /* { dg-error {passing 'int' to argument 2 of 'svbfmlalb_lane', which expects 'svbfloat16_t'} } */
  svbfmlalb_lane (f32, f32, bf16, 0); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svbfmlalb_lane', which expects 'svbfloat16_t'} } */
  svbfmlalb_lane (f32, bf16, 0, 0); /* { dg-error {passing 'int' to argument 3 of 'svbfmlalb_lane', which expects 'svbfloat16_t'} } */
  svbfmlalb_lane (f32, bf16, f32, 0); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svbfmlalb_lane', which expects 'svbfloat16_t'} } */
  svbfmlalb_lane (f32, bf16, bf16, s32); /* { dg-error {argument 4 of 'svbfmlalb_lane' must be an integer constant expression} } */
  svbfmlalb_lane (f32, bf16, bf16, i); /* { dg-error {argument 4 of 'svbfmlalb_lane' must be an integer constant expression} } */

  svbfmlalb_lane (f32, bf16, bf16, 0);
  svbfmlalb_lane (f32, bf16, bf16, 7);
  svbfmlalb_lane (f32, bf16, bf16, 8); /* { dg-error {passing 8 to argument 4 of 'svbfmlalb_lane', which expects a value in the range \[0, 7\]} } */
  svbfmlalb_lane (f32, bf16, bf16, -1); /* { dg-error {passing -1 to argument 4 of 'svbfmlalb_lane', which expects a value in the range \[0, 7\]} } */
}
