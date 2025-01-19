/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+lut" } */

#include "arm_neon.h"

void
test_var(uint8x16_t a, uint8x8_t b, uint8x16_t c, int x)
{
  vluti2q_lane_u8(a, b, x); /* { dg-error {argument 3 of 'vluti2q_lane_u8' must be an integer constant expression} } */
  vluti2q_laneq_u8(a, c, x); /* { dg-error {argument 3 of 'vluti2q_laneq_u8' must be an integer constant expression} } */
  vluti4q_lane_u8(a, b, x); /* { dg-error {argument 3 of 'vluti4q_lane_u8' must be an integer constant expression} } */
  vluti4q_laneq_u8(a, c, x); /* { dg-error {argument 3 of 'vluti4q_laneq_u8' must be an integer constant expression} } */
}

void
test_vluti2_laneu8(uint8x8_t a, uint8x8_t b, uint8x16_t c, uint8x16_t d)
{
  vluti2_lane_u8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2_lane_u8(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2_laneq_u8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_laneq_u8(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_lane_u8(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2q_lane_u8(c, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2q_laneq_u8(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_laneq_u8(c, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti2_lanes8(int8x8_t a, uint8x8_t b, int8x16_t c, uint8x16_t d)
{
  vluti2_lane_s8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2_lane_s8(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2_laneq_s8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_laneq_s8(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_lane_s8(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2q_lane_s8(c, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2q_laneq_s8(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_laneq_s8(c, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti2_lanep8(poly8x8_t a, uint8x8_t b, poly8x16_t c, uint8x16_t d)
{
  vluti2_lane_p8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2_lane_p8(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2_laneq_p8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_laneq_p8(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_lane_p8(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti2q_lane_p8(c, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti2q_laneq_p8(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_laneq_p8(c, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti2_laneu16(uint16x4_t a, uint8x8_t b, uint16x8_t c, uint8x16_t d)
{
  vluti2_lane_u16(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_lane_u16(a, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2_laneq_u16(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2_laneq_u16(a, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */

  vluti2q_lane_u16(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_lane_u16(c, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_laneq_u16(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2q_laneq_u16(c, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
}

void
test_vluti2_lanes16(int16x4_t a, uint8x8_t b, int16x8_t c, uint8x16_t d)
{
  vluti2_lane_s16(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_lane_s16(a, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2_laneq_s16(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2_laneq_s16(a, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */

  vluti2q_lane_s16(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_lane_s16(c, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_laneq_s16(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2q_laneq_s16(c, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
}

void
test_vluti2_lanep16(poly16x4_t a, uint8x8_t b, poly16x8_t c, uint8x16_t d)
{
  vluti2_lane_p16(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_lane_p16(a, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2_laneq_p16(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2_laneq_p16(a, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */

  vluti2q_lane_p16(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_lane_p16(c, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_laneq_p16(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2q_laneq_p16(c, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
}

void
test_vluti2_lanef16(float16x4_t a, uint8x8_t b, float16x8_t c, uint8x16_t d)
{
  vluti2_lane_f16(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_lane_f16(a, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2_laneq_f16(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2_laneq_f16(a, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */

  vluti2q_lane_f16(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_lane_f16(c, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_laneq_f16(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2q_laneq_f16(c, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
}

void
test_vluti2_lanebf16(bfloat16x4_t a, uint8x8_t b, bfloat16x8_t c, uint8x16_t d)
{
  vluti2_lane_bf16(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2_lane_bf16(a, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2_laneq_bf16(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2_laneq_bf16(a, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */

  vluti2q_lane_bf16(c, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti2q_lane_bf16(c, b, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */

  vluti2q_laneq_bf16(c, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
  vluti2q_laneq_bf16(c, d, 8); /* { dg-error {passing 8 to argument 3 [^\n]*, which expects a value in the range \[0, 7\]} } */
}

void
test_vluti4q_laneu8(uint8x16_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_u8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */
  vluti4q_lane_u8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */

  vluti4q_laneq_u8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_laneq_u8(a, d, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
}

void
test_vluti4q_lanes8(int8x16_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_s8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */
  vluti4q_lane_s8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */

  vluti4q_laneq_s8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_laneq_s8(a, d, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
}

void
test_vluti4q_lanep8(poly8x16_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_p8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */
  vluti4q_lane_p8(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects the value 0} } */

  vluti4q_laneq_p8(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_laneq_p8(a, d, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
}

void
test_vluti4q_laneu16_x2(uint16x8x2_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_u16_x2(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_lane_u16_x2(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti4q_laneq_u16_x2(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti4q_laneq_u16_x2(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti4q_lanes16_x2(int16x8x2_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_s16_x2(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_lane_s16_x2(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti4q_laneq_s16_x2(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti4q_laneq_s16_x2(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti4q_lanep16_x2(poly16x8x2_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_p16_x2(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_lane_p16_x2(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti4q_laneq_p16_x2(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti4q_laneq_p16_x2(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti4q_lanef16_x2(float16x8x2_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_f16_x2(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_lane_f16_x2(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti4q_laneq_f16_x2(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti4q_laneq_f16_x2(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}

void
test_vluti4q_lanebf16_x2(bfloat16x8x2_t a, uint8x8_t b, uint8x16_t d)
{
  vluti4q_lane_bf16_x2(a, b, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */
  vluti4q_lane_bf16_x2(a, b, 2); /* { dg-error {passing 2 to argument 3 [^\n]*, which expects a value in the range \[0, 1\]} } */

  vluti4q_laneq_bf16_x2(a, d, -1); /* { dg-error {passing -1 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
  vluti4q_laneq_bf16_x2(a, d, 4); /* { dg-error {passing 4 to argument 3 [^\n]*, which expects a value in the range \[0, 3\]} } */
}
