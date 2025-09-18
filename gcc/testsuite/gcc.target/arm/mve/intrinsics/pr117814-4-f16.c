/* Check that -mfp16-format=none works.  */

/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2 -mfp16-format=none" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float16x8_t
foo (float16_t const *base)
{
  return vld1q_f16 (base);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not {eabi_attribute 38,} } } */
