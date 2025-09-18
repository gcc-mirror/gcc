/* Check that we can compile if the target supports floating-point.  */

/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float32x4_t
foo (float32_t const *base)
{
  return vld1q_f32 (base);
}

#ifdef __cplusplus
}
#endif
