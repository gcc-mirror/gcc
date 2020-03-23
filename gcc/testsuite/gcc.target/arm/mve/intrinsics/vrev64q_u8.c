/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a)
{
    return vrev64q_u8 (a);
}

/* { dg-final { scan-assembler "vrev64.8"  }  } */

uint8x16_t
foo1 (uint8x16_t a)
{
    return vrev64q (a);
}

/* { dg-final { scan-assembler "vrev64.8"  }  } */
