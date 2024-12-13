/* { dg-do compile } */

#include "arm_neon.h"

#pragma GCC target "+fp8dot4+fp8dot2"

void
test(float16x4_t f16, float16x8_t f16q, float32x2_t f32,
     float32x4_t f32q, mfloat8x8_t mf8, mfloat8x16_t mf8q, int x,
     fpm_t fpm)
{
  vdot_lane_f16_mf8_fpm (f16, mf8, mf8, x, fpm); /* { dg-error {argument 4 of 'vdot_lane_f16_mf8_fpm' must be an integer constant expression} } */
  vdot_laneq_f16_mf8_fpm (f16, mf8, mf8q, x, fpm); /* { dg-error {argument 4 of 'vdot_laneq_f16_mf8_fpm' must be an integer constant expression} } */
  vdotq_lane_f16_mf8_fpm (f16q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vdotq_lane_f16_mf8_fpm' must be an integer constant expression} } */
  vdotq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vdotq_laneq_f16_mf8_fpm' must be an integer constant expression} } */

  vdot_lane_f32_mf8_fpm (f32, mf8, mf8, x, fpm); /* { dg-error {argument 4 of 'vdot_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vdot_laneq_f32_mf8_fpm (f32, mf8, mf8q, x, fpm); /* { dg-error {argument 4 of 'vdot_laneq_f32_mf8_fpm' must be an integer constant expression} } */
  vdotq_lane_f32_mf8_fpm (f32q, mf8q, mf8, x, fpm); /* { dg-error {argument 4 of 'vdotq_lane_f32_mf8_fpm' must be an integer constant expression} } */
  vdotq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, x, fpm); /* { dg-error {argument 4 of 'vdotq_laneq_f32_mf8_fpm' must be an integer constant expression} } */

  vdot_lane_f16_mf8_fpm (f16, mf8, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdot_lane_f16_mf8_fpm', which expects a value in the range \[0, 3\]} } */
  vdot_lane_f16_mf8_fpm (f16, mf8, mf8, 4, fpm); /* { dg-error { passing 4 to argument 4 of 'vdot_lane_f16_mf8_fpm', which expects a value in the range \[0, 3\]} } */

  vdot_laneq_f16_mf8_fpm (f16, mf8, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdot_laneq_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vdot_laneq_f16_mf8_fpm (f16, mf8, mf8q, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vdot_laneq_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */

  vdotq_lane_f16_mf8_fpm (f16q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdotq_lane_f16_mf8_fpm', which expects a value in the range \[0, 3\]} } */
  vdotq_lane_f16_mf8_fpm (f16q, mf8q, mf8, 4, fpm); /* { dg-error { passing 4 to argument 4 of 'vdotq_lane_f16_mf8_fpm', which expects a value in the range \[0, 3\]} } */

  vdotq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdotq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */
  vdotq_laneq_f16_mf8_fpm (f16q, mf8q, mf8q, 8, fpm); /* { dg-error { passing 8 to argument 4 of 'vdotq_laneq_f16_mf8_fpm', which expects a value in the range \[0, 7\]} } */

  vdot_lane_f32_mf8_fpm (f32, mf8, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdot_lane_f32_mf8_fpm', which expects a value in the range \[0, 1\]} } */
  vdot_lane_f32_mf8_fpm (f32, mf8, mf8, 2, fpm); /* { dg-error { passing 2 to argument 4 of 'vdot_lane_f32_mf8_fpm', which expects a value in the range \[0, 1\]} } */

  vdot_laneq_f32_mf8_fpm (f32, mf8, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdot_laneq_f32_mf8_fpm', which expects a value in the range \[0, 3\]} } */
  vdot_laneq_f32_mf8_fpm (f32, mf8, mf8q, 4, fpm); /* { dg-error { passing 4 to argument 4 of 'vdot_laneq_f32_mf8_fpm', which expects a value in the range \[0, 3\]} } */

  vdotq_lane_f32_mf8_fpm (f32q, mf8q, mf8, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdotq_lane_f32_mf8_fpm', which expects a value in the range \[0, 1\]} } */
  vdotq_lane_f32_mf8_fpm (f32q, mf8q, mf8, 2, fpm); /* { dg-error { passing 2 to argument 4 of 'vdotq_lane_f32_mf8_fpm', which expects a value in the range \[0, 1\]} } */

  vdotq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, -1, fpm); /* { dg-error { passing -1 to argument 4 of 'vdotq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 3\]} } */
  vdotq_laneq_f32_mf8_fpm (f32q, mf8q, mf8q, 4, fpm); /* { dg-error { passing 4 to argument 4 of 'vdotq_laneq_f32_mf8_fpm', which expects a value in the range \[0, 3\]} } */
}
