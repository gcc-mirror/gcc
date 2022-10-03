/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a, mve_pred16_t p)
{
  return vrev64q_m_s16 (a, a, p);
}

float16x8_t
foo2 (float16x8_t a, mve_pred16_t p)
{
  return vrev64q_m_f16 (a, a, p);
}
