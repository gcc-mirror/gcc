/* { dg-do compile } */
/* { dg-skip-if "" { arm*-*-* } } */
/* { dg-options "-O3" } */


#include <arm_neon.h>

float32x2_t foo_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  return vmls_f32 (a, b, c);
}

float32x4_t fooq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  return vmlsq_f32 (a, b, c);
}

float32x2_t foo_n_f32 (float32x2_t a, float32x2_t b, float32_t c)
{
  return vmls_n_f32 (a, b, c);
}

float32x4_t fooq_n_f32 (float32x4_t a, float32x4_t b, float32_t c)
{
  return vmlsq_n_f32 (a, b, c);
}

float32x2_t foo_lane_f32 (float32x2_t a,
			  float32x2_t b,
			  float32x2_t v)
{
  return vmls_lane_f32 (a, b, v, 0);
}

float32x4_t fooq_lane_f32 (float32x4_t a,
			   float32x4_t b,
			   float32x2_t v)
{
  return vmlsq_lane_f32 (a, b, v, 0);
}

float32x2_t foo_laneq_f32 (float32x2_t a,
			   float32x2_t b,
			   float32x4_t v)
{
  return vmls_laneq_f32 (a, b, v, 0);
}

float32x4_t fooq_laneq_f32 (float32x4_t a,
			    float32x4_t b,
			    float32x4_t v)
{
  return vmlsq_laneq_f32 (a, b, v, 0);
}

float64x1_t foo_f64 (float64x1_t a, float64x1_t b, float64x1_t c)
{
  return vmls_f64 (a, b, c);
}

float64x2_t fooq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  return vmlsq_f64 (a, b, c);
}

/* { dg-final { scan-assembler-times {\tfmul\t} 10} }  */
/* { dg-final { scan-assembler-times {\tfsub\t} 10} }  */
