/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

int64x2_t
foo (int64_t a, int64x2_t b)
{
  return vsetq_lane_s64 (a, b, 2); /* { dg-error {passing 2 to argument 3 of 'vsetq_lane_s64', which expects a value in the range \[0, 1\]} } */
}

#ifdef __cplusplus
}
#endif
