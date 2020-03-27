/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
uqshl_imm (uint32_t longval3)
{
  return uqshl (longval3, 21);
}

/* { dg-final { scan-assembler "uqshl\\tr\[0-9\]+, #21" } } */
