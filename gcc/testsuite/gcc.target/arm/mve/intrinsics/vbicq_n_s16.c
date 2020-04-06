/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a)
{
  return vbicq_n_s16 (a, 1);
}

int16x8_t
foo1 (int16x8_t a)
{
  return vbicq (a, 1);
}

/* { dg-final { scan-assembler-times "vbic.i16" 2 }  } */
