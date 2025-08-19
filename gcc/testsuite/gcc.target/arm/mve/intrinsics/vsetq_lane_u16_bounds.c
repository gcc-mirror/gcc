/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

uint16x8_t
foo (uint16_t a, uint16x8_t b)
{
  return vsetq_lane_u16 (a, b, 8); /* { dg-error {passing 8 to argument 3 of 'vsetq_lane_u16', which expects a value in the range \[0, 7\]} } */
}

#ifdef __cplusplus
}
#endif
