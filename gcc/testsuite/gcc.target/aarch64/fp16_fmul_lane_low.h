#include "arm_neon.h"

float32x2_t
test_vfmlal_lane_low_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlal_lane_low_f16 (r, a, b, 0);
}

float32x2_t
test_vfmlsl_lane_low_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlsl_lane_low_f16 (r, a, b, 0);
}

float32x2_t
test_vfmlal_laneq_low_f16 (float32x2_t r, float16x4_t a, float16x8_t b)
{
  return vfmlal_laneq_low_f16 (r, a, b, 6);
}

float32x2_t
test_vfmlsl_laneq_low_f16 (float32x2_t r, float16x4_t a, float16x8_t b)
{
  return vfmlsl_laneq_low_f16 (r, a, b, 6);
}

float32x4_t
test_vfmlalq_lane_low_f16 (float32x4_t r, float16x8_t a, float16x4_t b)
{
  return vfmlalq_lane_low_f16 (r, a, b, 1);
}

float32x4_t
test_vfmlslq_lane_low_f16 (float32x4_t r, float16x8_t a, float16x4_t b)
{
  return vfmlslq_lane_low_f16 (r, a, b, 1);
}

float32x4_t
test_vfmlalq_laneq_low_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlalq_laneq_low_f16 (r, a, b, 7);
}

float32x4_t
test_vfmlslq_laneq_low_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlslq_laneq_low_f16 (r, a, b, 7);
}
