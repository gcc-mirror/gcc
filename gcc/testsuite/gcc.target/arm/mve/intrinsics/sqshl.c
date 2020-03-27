/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
sqshl_imm (int32_t longval3)
{
  return sqshl (longval3, 25);
}

/* { dg-final { scan-assembler "sqshl\\tr\[0-9\]+, #25" } } */
