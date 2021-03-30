/* PR target/99808 */
/* PR target/99037 */
/* { dg-do compile } */
/* { dg-options "-Og -fweb -fno-forward-propagate -g" } */

#include <arm_neon.h>

float32x4_t
foo (void)
{
  float64x2_t arg2 = vcombine_f64 ((float64x1_t) 0ULL, (float64x1_t) 1ULL);
  return vcvt_high_f32_f64 ((float32x2_t) 1ULL, arg2);
}

