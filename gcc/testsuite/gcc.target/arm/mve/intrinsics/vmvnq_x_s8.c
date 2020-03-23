/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8x16_t a, mve_pred16_t p)
{
  return vmvnq_x_s8 (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmvnt"  }  } */

int8x16_t
foo1 (int8x16_t a, mve_pred16_t p)
{
  return vmvnq_x (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmvnt"  }  } */
