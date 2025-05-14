/* { dg-do compile } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>

float32x2_t foo_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  return vmla_f32 (a, b, c);
}

float32x4_t fooq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  return vmlaq_f32 (a, b, c);
}

float32x2_t foo_n_f32 (float32x2_t a, float32x2_t b, float32_t c)
{
  return vmla_n_f32 (a, b, c);
}

float32x4_t fooq_n_f32 (float32x4_t a, float32x4_t b, float32_t c)
{
  return vmlaq_n_f32 (a, b, c);
}

float32x2_t foo_lane_f32 (float32x2_t a,
			  float32x2_t b,
			  float32x2_t v)
{
  return vmla_lane_f32 (a, b, v, 0);
}

float32x4_t fooq_lane_f32 (float32x4_t a,
			   float32x4_t b,
			   float32x2_t v)
{
  return vmlaq_lane_f32 (a, b, v, 0);
}

float32x2_t foo_laneq_f32 (float32x2_t a,
			   float32x2_t b,
			   float32x4_t v)
{
  return vmla_laneq_f32 (a, b, v, 0);
}

float32x4_t fooq_laneq_f32 (float32x4_t a,
			    float32x4_t b,
			    float32x4_t v)
{
  return vmlaq_laneq_f32 (a, b, v, 0);
}

float64x1_t foo_f64 (float64x1_t a, float64x1_t b, float64x1_t c)
{
  return vmla_f64 (a, b, c);
}

float64x2_t fooq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  return vmlaq_f64 (a, b, c);
}

/* { dg-final { scan-assembler-times {\tfmul\t} 10} }  */
/* { dg-final { scan-assembler-times {\tfadd\t} 10} }  */
