/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t inactive, mve_pred16_t p)
{
  return vmvnq_m_n_u16 (inactive, 4, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmvnt.i16"  }  } */

uint16x8_t
foo1 (uint16x8_t inactive, mve_pred16_t p)
{
  return vmvnq_m (inactive, 4, p);
}

/* { dg-final { scan-assembler "vpst" } } */
