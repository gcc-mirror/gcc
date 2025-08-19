/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

uint64x2_t
foo (uint64_t a, uint64x2_t b)
{
  return vsetq_lane_u64 (a, b, 2); /* { dg-error {passing 2 to argument 3 of 'vsetq_lane_u64', which expects a value in the range \[0, 1\]} } */
}

#ifdef __cplusplus
}
#endif
