/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

int16x8_t
foo (int16_t a, int16x8_t b)
{
  return vsetq_lane_s16 (a, b, 9); /* { dg-error {passing 9 to argument 3 of 'vsetq_lane_s16', which expects a value in the range \[0, 7\]} } */
}

#ifdef __cplusplus
}
#endif
