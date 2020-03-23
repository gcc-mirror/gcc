/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
sqshll_imm(int64_t value)
{
  return sqshll (value, 21);
}

/* { dg-final { scan-assembler "sqshll\\tr\[0-9\]+, r\[0-9\]+, #21" } } */
