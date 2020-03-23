/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8_t a)
{
    return vdupq_n_u8 (a);
}

/* { dg-final { scan-assembler "vdup.8"  }  } */
