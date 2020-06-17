/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint8x16_t a, uint8x16_t b, mve_pred16_t p)
{
  return vmladavq_p_u8 (a, b, p);
}

/* { dg-final { scan-assembler "vmladavt.u8"  }  } */

uint32_t
foo1 (uint8x16_t a, uint8x16_t b, mve_pred16_t p)
{
  return vmladavq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmladavt.u8"  }  } */
