/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (uint32_t a)
{
  return vctp16q (a);
}

/* { dg-final { scan-assembler "vctp.16"  }  } */

mve_pred16_t
foo1 (uint32_t a)
{
  return vctp16q (a);
}

/* { dg-final { scan-assembler "vctp.16"  }  } */
