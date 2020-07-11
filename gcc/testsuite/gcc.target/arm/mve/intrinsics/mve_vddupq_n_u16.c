/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo1 (int32_t a)
{
  return vddupq_u16 (a, 4);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
