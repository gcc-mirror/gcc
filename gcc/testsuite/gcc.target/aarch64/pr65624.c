/* { dg-do compile } */

#include <arm_neon.h>

int32x4_t
foo (void)
{
  int32x4_t vector_int32x4;
  int16x4_t vector3_int16x4;
  int16x4_t vector4_int16x4;
  static int32_t buffer_int32x4[32];

  vector_int32x4 = vld1q_s32(buffer_int32x4);
  return vqdmlsl_lane_s16(vector_int32x4, vector3_int16x4, vector4_int16x4, 0);
}
