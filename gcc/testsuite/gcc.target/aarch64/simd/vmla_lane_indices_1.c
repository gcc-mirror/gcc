/* { dg-do compile } */

#include "arm_neon.h"

#pragma GCC target "+fp8dot4+fp8dot2"

void
test(float16x4_t f16, float16x8_t f16q, float32x2_t f32,
     float32x4_t f32q, mfloat8x8_t mf8, mfloat8x16_t mf8q, int x,
     fpm_t fpm)
{
  vmlalbq_lane_f16_mf8_fpm (f16q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlalbq_lane_f16_mf8_fpm' must be an integer constant expression} } */
  vmlalbq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlalbq_laneq_f16_mf8_fpm' must be an integer constant expression} } */
  vmlaltq_lane_f16_mf8_fpm (f16q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlaltq_lane_f16_mf8_fpm' must be an integer constant expression} } */
  vmlaltq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlaltq_laneq_f16_mf8_fpm' must be an integer constant expression} } */

  vmlallbbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlallbbq_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vmlallbbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlallbbq_laneq_f32_mf8_fpm' must be an integer constant expression} } */
  vmlallbtq_lane_f32_mf8_fpm (f32q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlallbtq_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vmlallbtq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlallbtq_laneq_f32_mf8_fpm' must be an integer constant expression} } */
  vmlalltbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlalltbq_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vmlalltbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlalltbq_laneq_f32_mf8_fpm' must be an integer constant expression} } */
  vmlallttq_lane_f32_mf8_fpm (f32q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vmlallttq_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vmlallttq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vmlallttq_laneq_f32_mf8_fpm' must be an integer constant expression} } */

  vmlalbq_lane_f16_mf8_fpm (f16q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlalbq_lane_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlalbq_lane_f16_mf8_fpm (f16q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlalbq_lane_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlalbq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlalbq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlalbq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlalbq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 15\]} } */

  vmlaltq_lane_f16_mf8_fpm (f16q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlaltq_lane_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlaltq_lane_f16_mf8_fpm (f16q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlaltq_lane_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlaltq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlaltq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlaltq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlaltq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 15\]} } */

  vmlallbbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallbbq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallbbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlallbbq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallbbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallbbq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlallbbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlallbbq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */

  vmlallbtq_lane_f32_mf8_fpm (f32q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallbtq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallbtq_lane_f32_mf8_fpm (f32q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlallbtq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallbtq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallbtq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlallbtq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlallbtq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */

  vmlalltbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlalltbq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlalltbq_lane_f32_mf8_fpm (f32q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlalltbq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlalltbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlalltbq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlalltbq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlalltbq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */

  vmlallttq_lane_f32_mf8_fpm (f32q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallttq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallttq_lane_f32_mf8_fpm (f32q, mf8q, mf8, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vmlallttq_lane_f32_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vmlallttq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vmlallttq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */
  vmlallttq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, 16, fpm); /* { dg-error { passing 16 to argument 4 of 'vmlallttq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 15\]} } */
}
