/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv9.2-a+sve2+lut")

void
test (svfloat16_t f16, svfloat16x2_t f16x2,
      svuint8_t u8, svuint16_t u16, svuint8x2_t u8x2, svuint16x2_t u16x2,
      svint8_t s8, svint16_t s16, svint8x2_t s8x2, svint16x2_t s16x2,
      svbfloat16_t bf16, svbfloat16x2_t bf16x2)
{
  svluti2_lane_f16 (f16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_f16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane_f16 (f16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane_f16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (f16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (f16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */

  svluti2_lane_bf16 (bf16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_bf16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane_bf16 (bf16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane_bf16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (bf16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (bf16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */

  svluti2_lane_u8 (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_u8', which expects a value in the range \[0, 3\]} } */
  svluti2_lane_u8 (u8, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti2_lane_u8', which expects a value in the range \[0, 3\]} } */
  svluti2_lane (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 3\]} } */
  svluti2_lane (u8, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 3\]} } */

  svluti2_lane_u16 (u16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_u16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane_u16 (u16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane_u16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (u16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (u16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */

  svluti2_lane_s8 (s8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_s8', which expects a value in the range \[0, 3\]} } */
  svluti2_lane_s8 (s8, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti2_lane_s8', which expects a value in the range \[0, 3\]} } */
  svluti2_lane (s8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 3\]} } */
  svluti2_lane (s8, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 3\]} } */

  svluti2_lane_s16 (s16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane_s16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane_s16 (s16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane_s16', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (s16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */
  svluti2_lane (s16, u8, 8); /* { dg-error {passing 8 to argument 3 of 'svluti2_lane', which expects a value in the range \[0, 7\]} } */

  svluti4_lane_f16 (f16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_f16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_f16 (f16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_f16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (f16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (f16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_f16_x2 (f16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_f16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_f16_x2 (f16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_f16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (f16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (f16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_bf16 (bf16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_bf16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_bf16 (bf16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_bf16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (bf16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (bf16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_bf16_x2 (bf16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_bf16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_bf16_x2 (bf16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_bf16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (bf16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (bf16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_u8 (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_u8', which expects a value in the range \[0, 1\]} } */
  svluti4_lane_u8 (u8, u8, 2); /* { dg-error {passing 2 to argument 3 of 'svluti4_lane_u8', which expects a value in the range \[0, 1\]} } */
  svluti4_lane (u8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 1\]} } */
  svluti4_lane (u8, u8, 2); /* { dg-error {passing 2 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 1\]} } */

  svluti4_lane_u16 (u16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_u16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_u16 (u16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_u16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (u16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (u16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_u16_x2 (u16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_u16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_u16_x2 (u16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_u16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (u16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (u16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_s8 (s8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_s8', which expects a value in the range \[0, 1\]} } */
  svluti4_lane_s8 (s8, u8, 2); /* { dg-error {passing 2 to argument 3 of 'svluti4_lane_s8', which expects a value in the range \[0, 1\]} } */
  svluti4_lane (s8, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 1\]} } */
  svluti4_lane (s8, u8, 2); /* { dg-error {passing 2 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 1\]} } */

  svluti4_lane_s16 (s16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_s16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_s16 (s16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_s16', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (s16, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (s16, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */

  svluti4_lane_s16_x2 (s16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane_s16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane_s16_x2 (s16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane_s16_x2', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (s16x2, u8, -1); /* { dg-error {passing -1 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
  svluti4_lane (s16x2, u8, 4); /* { dg-error {passing 4 to argument 3 of 'svluti4_lane', which expects a value in the range \[0, 3\]} } */
}
