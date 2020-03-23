/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
uqshll_imm(uint64_t value)
{
  return uqshll (value, 21);
}

/* { dg-final { scan-assembler "uqshll\\tr\[0-9\]+, r\[0-9\]+, #21" } } */
