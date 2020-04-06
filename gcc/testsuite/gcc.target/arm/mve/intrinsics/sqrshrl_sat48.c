/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
sqrshrl_reg (int64_t longval3, int32_t x)
{
  return sqrshrl_sat48 (longval3, x);
}

/* { dg-final { scan-assembler "sqrshrl\\tr\[0-9\]+, r\[0-9\]+, #48, r\[0-9\]+" } } */
