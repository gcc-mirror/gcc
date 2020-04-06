/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
srshr_imm (int32_t longval3)
{
  return srshr (longval3, 25);
}

/* { dg-final { scan-assembler "srshr\\tr\[0-9\]+, #25" } } */
