/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>

uint32x4_t foo (uint32x4_t a, uint32x4_t b)
{
  mve_pred16_t p = vcmpneq_n_u32 (vandq_u32 (a, b), 0);
  return vaddq_x_u32 (a, b, p);
}
