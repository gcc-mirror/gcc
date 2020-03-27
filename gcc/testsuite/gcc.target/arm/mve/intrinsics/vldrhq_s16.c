/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16_t const * base)
{
  return vldrhq_s16 (base);
}

/* { dg-final { scan-assembler "vldrh.s16"  }  } */
