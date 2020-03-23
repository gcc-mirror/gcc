/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16_t a)
{
    return vdupq_n_u16 (a);
}

/* { dg-final { scan-assembler "vdup.16"  }  } */
