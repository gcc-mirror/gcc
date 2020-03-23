/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a)
{
  return vorrq_n_u16 (a, 1);
}

/* { dg-final { scan-assembler "vorr.i16"  }  } */
