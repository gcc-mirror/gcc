/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (mve_pred16_t a)
{
  return vpnot (a);
}

/* { dg-final { scan-assembler "vpnot"  }  } */

mve_pred16_t
foo1 (mve_pred16_t a)
{
  return vpnot (a);
}

/* { dg-final { scan-assembler "vpnot"  }  } */
