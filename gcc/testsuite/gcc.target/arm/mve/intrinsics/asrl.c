/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
asrl_reg (int64_t longval3, int32_t x)
{
  return asrl (longval3, x);
}

/* { dg-final { scan-assembler "asrl\\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+" } } */
