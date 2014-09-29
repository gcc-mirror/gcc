/* { dg-do compile } */
/* { dg-options "-O0" } */

#include "arm_neon.h"

float64_t
wrap_vmuld_lane_f64 (float64_t a, float64x1_t b)
{
  return vmuld_lane_f64 (a, b, 0);
}

float64_t
wrap_vmuld_laneq_f64 (float64_t a, float64x2_t b)
{
  return vmuld_laneq_f64 (a, b, 0);
}

float32_t
wrap_vmuls_lane_f32 (float32_t a, float32x2_t b)
{
  return vmuls_lane_f32 (a, b, 0);
}

float32_t
wrap_vmuls_laneq_f32 (float32_t a, float32x4_t b)
{
  return vmuls_laneq_f32 (a, b, 0);
}
