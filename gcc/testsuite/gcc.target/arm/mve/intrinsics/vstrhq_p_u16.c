/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint16_t * addr, uint16x8_t value, mve_pred16_t p)
{
  vstrhq_p_u16 (addr, value, p);
}

/* { dg-final { scan-assembler "vstrht.16"  }  } */

void
foo1 (uint16_t * addr, uint16x8_t value, mve_pred16_t p)
{
  vstrhq_p (addr, value, p);
}

/* { dg-final { scan-assembler "vstrht.16"  }  } */
