/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint32_t a, uint16x8_t b, uint16x8_t c, mve_pred16_t p)
{
  return vmladavaq_p_u16 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmladavat.u16"  }  } */

uint32_t
foo1 (uint32_t a, uint16x8_t b, uint16x8_t c, mve_pred16_t p)
{
  return vmladavaq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmladavat.u16"  }  } */
/* { dg-final { scan-assembler "vmladavat.u16"  }  } */
