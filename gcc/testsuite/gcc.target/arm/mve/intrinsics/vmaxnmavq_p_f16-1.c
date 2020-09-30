/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
float16_t
foo1 (float16_t a, float16x8_t b, mve_pred16_t p)
{
  return vmaxnmavq_p (a, b, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
