/* { dg-do assemble } */
/* { dg-additional-options "-O -std=gnu23 --save-temps" } */

#include <arm_neon.h>

void test(mfloat8x8_t x8, mfloat8x16_t x16,
	  mfloat8x8x2_t x8x2, mfloat8x16x2_t x16x2,
	  mfloat8x8x3_t x8x3, mfloat8x16x3_t x16x3,
	  mfloat8x8x4_t x8x4, mfloat8x16x4_t x16x4,
	  mfloat8_t *ptr, mfloat8_t scalar)
{
  vcopy_lane_mf8(x8, -1, x8, 0); /* { dg-error {passing -1 to argument 2 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_lane_mf8(x8, 8, x8, 0); /* { dg-error {passing 8 to argument 2 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_lane_mf8(x8, 0, x8, -1); /* { dg-error {passing -1 to argument 4 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_lane_mf8(x8, 0, x8, 8); /* { dg-error {passing 8 to argument 4 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_lane_mf8(x8, 100, x8, 100); /* { dg-error {passing 100 to argument 2 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} } */
  /* { dg-error {passing 100 to argument 4 of 'vcopy_lane_mf8', which expects a value in the range \[0, 7\]} "" { target *-*-* } .-1 } */

  vcopy_laneq_mf8(x8, -1, x16, 0); /* { dg-error {passing -1 to argument 2 of 'vcopy_laneq_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_laneq_mf8(x8, 8, x16, 0); /* { dg-error {passing 8 to argument 2 of 'vcopy_laneq_mf8', which expects a value in the range \[0, 7\]} } */
  vcopy_laneq_mf8(x8, 0, x16, -1); /* { dg-error {passing -1 to argument 4 of 'vcopy_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vcopy_laneq_mf8(x8, 0, x16, 16); /* { dg-error {passing 16 to argument 4 of 'vcopy_laneq_mf8', which expects a value in the range \[0, 15\]} } */

  vcopyq_lane_mf8(x16, -1, x8, 0); /* { dg-error {passing -1 to argument 2 of 'vcopyq_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vcopyq_lane_mf8(x16, 16, x8, 0); /* { dg-error {passing 16 to argument 2 of 'vcopyq_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vcopyq_lane_mf8(x16, 0, x8, -1); /* { dg-error {passing -1 to argument 4 of 'vcopyq_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vcopyq_lane_mf8(x16, 0, x8, 8); /* { dg-error {passing 8 to argument 4 of 'vcopyq_lane_mf8', which expects a value in the range \[0, 7\]} } */

  vcopyq_laneq_mf8(x16, -1, x16, 0); /* { dg-error {passing -1 to argument 2 of 'vcopyq_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vcopyq_laneq_mf8(x16, 16, x16, 0); /* { dg-error {passing 16 to argument 2 of 'vcopyq_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vcopyq_laneq_mf8(x16, 0, x16, -1); /* { dg-error {passing -1 to argument 4 of 'vcopyq_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vcopyq_laneq_mf8(x16, 0, x16, 16); /* { dg-error {passing 16 to argument 4 of 'vcopyq_laneq_mf8', which expects a value in the range \[0, 15\]} } */

  vdup_lane_mf8(x8, -1); /* { dg-error {passing -1 to argument 2 of 'vdup_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdup_lane_mf8(x8, 8); /* { dg-error {passing 8 to argument 2 of 'vdup_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdup_laneq_mf8(x16, -1); /* { dg-error {passing -1 to argument 2 of 'vdup_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vdup_laneq_mf8(x16, 16); /* { dg-error {passing 16 to argument 2 of 'vdup_laneq_mf8', which expects a value in the range \[0, 15\]} } */

  vdupq_lane_mf8(x8, -1); /* { dg-error {passing -1 to argument 2 of 'vdupq_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdupq_lane_mf8(x8, 8); /* { dg-error {passing 8 to argument 2 of 'vdupq_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdupq_laneq_mf8(x16, -1); /* { dg-error {passing -1 to argument 2 of 'vdupq_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vdupq_laneq_mf8(x16, 16); /* { dg-error {passing 16 to argument 2 of 'vdupq_laneq_mf8', which expects a value in the range \[0, 15\]} } */

  vdupb_lane_mf8(x8, -1); /* { dg-error {passing -1 to argument 2 of 'vdupb_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdupb_lane_mf8(x8, 8); /* { dg-error {passing 8 to argument 2 of 'vdupb_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vdupb_laneq_mf8(x16, -1); /* { dg-error {passing -1 to argument 2 of 'vdupb_laneq_mf8', which expects a value in the range \[0, 15\]} } */
  vdupb_laneq_mf8(x16, 16); /* { dg-error {passing 16 to argument 2 of 'vdupb_laneq_mf8', which expects a value in the range \[0, 15\]} } */

  vext_mf8(x8, x8, -1); /* { dg-error {passing -1 to argument 3 of 'vext_mf8', which expects a value in the range \[0, 7\]} } */
  vext_mf8(x8, x8, 8); /* { dg-error {passing 8 to argument 3 of 'vext_mf8', which expects a value in the range \[0, 7\]} } */
  vextq_mf8(x16, x16, -1); /* { dg-error {passing -1 to argument 3 of 'vextq_mf8', which expects a value in the range \[0, 15\]} } */
  vextq_mf8(x16, x16, 16); /* { dg-error {passing 16 to argument 3 of 'vextq_mf8', which expects a value in the range \[0, 15\]} } */

  vld1_lane_mf8(ptr, x8, -1); /* { dg-error {passing -1 to argument 3 of 'vld1_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld1_lane_mf8(ptr, x8, 8); /* { dg-error {passing 8 to argument 3 of 'vld1_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld1q_lane_mf8(ptr, x16, -1); /* { dg-error {passing -1 to argument 3 of 'vld1q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vld1q_lane_mf8(ptr, x16, 16); /* { dg-error {passing 16 to argument 3 of 'vld1q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vld2_lane_mf8(ptr, x8x2, -1); /* { dg-error {passing -1 to argument 3 of 'vld2_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld2_lane_mf8(ptr, x8x2, 8); /* { dg-error {passing 8 to argument 3 of 'vld2_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld2q_lane_mf8(ptr, x16x2, -1); /* { dg-error {passing -1 to argument 3 of 'vld2q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vld2q_lane_mf8(ptr, x16x2, 16); /* { dg-error {passing 16 to argument 3 of 'vld2q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vld3_lane_mf8(ptr, x8x3, -1); /* { dg-error {passing -1 to argument 3 of 'vld3_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld3_lane_mf8(ptr, x8x3, 8); /* { dg-error {passing 8 to argument 3 of 'vld3_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld3q_lane_mf8(ptr, x16x3, -1); /* { dg-error {passing -1 to argument 3 of 'vld3q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vld3q_lane_mf8(ptr, x16x3, 16); /* { dg-error {passing 16 to argument 3 of 'vld3q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vld4_lane_mf8(ptr, x8x4, -1); /* { dg-error {passing -1 to argument 3 of 'vld4_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld4_lane_mf8(ptr, x8x4, 8); /* { dg-error {passing 8 to argument 3 of 'vld4_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vld4q_lane_mf8(ptr, x16x4, -1); /* { dg-error {passing -1 to argument 3 of 'vld4q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vld4q_lane_mf8(ptr, x16x4, 16); /* { dg-error {passing 16 to argument 3 of 'vld4q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vset_lane_mf8(scalar, x8, -1); /* { dg-error {passing -1 to argument 3 of 'vset_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vset_lane_mf8(scalar, x8, 8); /* { dg-error {passing 8 to argument 3 of 'vset_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vsetq_lane_mf8(scalar, x16, -1); /* { dg-error {passing -1 to argument 3 of 'vsetq_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vsetq_lane_mf8(scalar, x16, 16); /* { dg-error {passing 16 to argument 3 of 'vsetq_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vst1_lane_mf8(ptr, x8, -1); /* { dg-error {passing -1 to argument 3 of 'vst1_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst1_lane_mf8(ptr, x8, 8); /* { dg-error {passing 8 to argument 3 of 'vst1_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst1q_lane_mf8(ptr, x16, -1); /* { dg-error {passing -1 to argument 3 of 'vst1q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vst1q_lane_mf8(ptr, x16, 16); /* { dg-error {passing 16 to argument 3 of 'vst1q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vst2_lane_mf8(ptr, x8x2, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst2_lane_mf8(ptr, x8x2, 8); /* { dg-error {passing 8 to argument 3 of 'vst2_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst2q_lane_mf8(ptr, x16x2, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vst2q_lane_mf8(ptr, x16x2, 16); /* { dg-error {passing 16 to argument 3 of 'vst2q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vst3_lane_mf8(ptr, x8x3, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst3_lane_mf8(ptr, x8x3, 8); /* { dg-error {passing 8 to argument 3 of 'vst3_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst3q_lane_mf8(ptr, x16x3, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vst3q_lane_mf8(ptr, x16x3, 16); /* { dg-error {passing 16 to argument 3 of 'vst3q_lane_mf8', which expects a value in the range \[0, 15\]} } */

  vst4_lane_mf8(ptr, x8x4, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst4_lane_mf8(ptr, x8x4, 8); /* { dg-error {passing 8 to argument 3 of 'vst4_lane_mf8', which expects a value in the range \[0, 7\]} } */
  vst4q_lane_mf8(ptr, x16x4, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_mf8', which expects a value in the range \[0, 15\]} } */
  vst4q_lane_mf8(ptr, x16x4, 16); /* { dg-error {passing 16 to argument 3 of 'vst4q_lane_mf8', which expects a value in the range \[0, 15\]} } */
}
