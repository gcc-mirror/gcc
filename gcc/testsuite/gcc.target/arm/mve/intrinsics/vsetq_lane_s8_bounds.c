/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

int8x16_t
foo (int8_t a, int8x16_t b)
{
  return vsetq_lane_s8 (a, b, 17); /* { dg-error {passing 17 to argument 3 of 'vsetq_lane_s8', which expects a value in the range \[0, 15\]} } */
}

#ifdef __cplusplus
}
#endif
