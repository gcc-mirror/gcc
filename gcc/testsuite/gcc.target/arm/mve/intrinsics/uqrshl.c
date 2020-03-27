/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
uqrshl_reg (uint32_t longval3, int32_t x)
{
  return uqrshl (longval3, x);
}

/* { dg-final { scan-assembler "uqrshl\\tr\[0-9\]+, r\[0-9\]+" } } */
