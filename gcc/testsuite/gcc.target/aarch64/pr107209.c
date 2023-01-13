/* { dg-options "-O2 -fnon-call-exceptions -fno-tree-fre" } */

#include <arm_neon.h>

float64x1_t
foo (void)
{
  float64_t v1 = 3.14159265359;
  float64_t v2 = 1.383894;
  float64_t vec_1_data[] = {v1};
  float64_t vec_2_data[] = {v2};
  float64x1_t vec_1 = vld1_f64 (vec_1_data);
  float64x1_t vec_2 = vld1_f64 (vec_2_data);

  return vmulx_f64 (vec_1, vec_2);
}
