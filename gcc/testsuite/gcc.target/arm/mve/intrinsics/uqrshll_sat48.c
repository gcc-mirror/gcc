/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
uqrshll_reg (uint64_t longval3, int32_t x)
{
  return uqrshll_sat48 (longval3, x);
}

/* { dg-final { scan-assembler "uqrshll\\tr\[0-9\]+, r\[0-9\]+, #48, r\[0-9\]+" } } */
