/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** foo:
**      fcvtn	v0.4h, v0.4s
**      ret
*/

float16x8_t
foo (float32x4_t a)
{
  float16x4_t b = vcvt_f16_f32 (a);
  return vcombine_f16 (b, vdup_n_f16 (0.0));
}

/*
** foo_d:
**      fcvtn	v0.2s, v0.2d
**      ret
*/

float32x4_t
foo_d (float64x2_t a)
{
  float32x2_t b = vcvt_f32_f64 (a);
  return vcombine_f32 (b, vdup_n_f32 (0.0));
}

