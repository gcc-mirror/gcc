/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float32x4_t
foo (float32_t a, float32x4_t b)
{
  return vsetq_lane_f32 (a, b, 4); /* { dg-error {passing 4 to argument 3 of 'vsetq_lane_f32', which expects a value in the range \[0, 3\]} } */
}

#ifdef __cplusplus
}
#endif
