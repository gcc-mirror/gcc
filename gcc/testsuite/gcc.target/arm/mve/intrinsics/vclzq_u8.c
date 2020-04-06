/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a)
{
    return vclzq_u8 (a);
}

/* { dg-final { scan-assembler "vclz.i8"  }  } */

uint8x16_t
foo1 (uint8x16_t a)
{
    return vclzq (a);
}

/* { dg-final { scan-assembler "vclz.i8"  }  } */
