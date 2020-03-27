/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a, uint32x4_t b, unsigned * carry_out)
{
  return vadciq_u32 (a, b, carry_out);
}

/* { dg-final { scan-assembler "vadci.i32"  }  } */

uint32x4_t
foo1 (uint32x4_t a, uint32x4_t b, unsigned * carry_out)
{
  return vadciq (a, b, carry_out);
}

/* { dg-final { scan-assembler "vadci.i32"  }  } */
