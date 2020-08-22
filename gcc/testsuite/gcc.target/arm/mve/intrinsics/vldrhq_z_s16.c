/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16_t const * base, mve_pred16_t p)
{
  return vldrhq_z_s16 (base, p);
}

/* { dg-final { scan-assembler-times "vpst" 1 }  } */
/* { dg-final { scan-assembler-times "vldrht.16" 1 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
