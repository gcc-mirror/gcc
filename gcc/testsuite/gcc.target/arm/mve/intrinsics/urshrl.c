/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
urshrl_imm(uint64_t value)
{
  return urshrl (value, 21);
}

/* { dg-final { scan-assembler "urshrl\\tr\[0-9\]+, r\[0-9\]+, #21" } } */
