/* PR target/64821 */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
#include <arm_neon.h>

/* Check that we lower __builtin_aarch64_sqrt* into the internal function SQRT. */
/* { dg-final { scan-tree-dump-times " __builtin_aarch64_sqrt" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\.SQRT " 4 "optimized" } } */

float64x1_t f64(float64x1_t a)
{
      return vsqrt_f64 (a);
}

float64x2_t f64q(float64x2_t a)
{
      return vsqrtq_f64 (a);
}

float32x2_t f32(float32x2_t a)
{
      return vsqrt_f32 (a);
}

float32x4_t f32q(float32x4_t a)
{
      return vsqrtq_f32 (a);
}
