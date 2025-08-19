/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float16x8_t
foo (float16_t a, float16x8_t b)
{
  return vsetq_lane_f16 (a, b, 9); /* { dg-error {passing 9 to argument 3 of 'vsetq_lane_f16', which expects a value in the range \[0, 7\]} } */
}

#ifdef __cplusplus
}
#endif
