/* Check that we get an error if the target does not support floating-point: we
   force +mve to cancel a possible implicit +mve.fp.  */

/* { dg-require-effective-target arm_v8_1m_mve_nofp_ok } */
/* { dg-add-options arm_v8_1m_mve_nofp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float32x4_t
foo (float32_t const *base)
{
  return vld1q_f32 (base);  /* { dg-error {ACLE function '.*vld1q_f32.*' requires ISA extension 'mve.fp'} } */
}

#ifdef __cplusplus
}
#endif
