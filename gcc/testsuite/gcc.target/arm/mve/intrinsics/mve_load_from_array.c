/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (void)
{
  static uint16_t const a[] = {0, 1, 2, 3, 4, 5, 6, 7};
  return vld1q (a);
}

uint16_t b[] = {0, 1, 2, 3, 4, 5, 6, 7};
void
bar (uint16x8_t value)
{
  vst1q (b, value);
}
