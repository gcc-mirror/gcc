/* Check that we get an error if the target does not support floating-point: we
   force +mve to cancel a possible implicit +mve.fp.  */

/* { dg-require-effective-target arm_v8_1m_mve_nofp_ok } */
/* { dg-add-options arm_v8_1m_mve_nofp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float16x8_t
foo (float16_t const *base)
{
  return vld1q_f16 (base);  /* { dg-error {ACLE function '.*vld1q_f16.*' requires ISA extension 'mve.fp'} } */
}

#ifdef __cplusplus
}
#endif
