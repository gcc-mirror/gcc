/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a)
{
    return vrev64q_u16 (a);
}

/* { dg-final { scan-assembler "vrev64.16"  }  } */

uint16x8_t
foo1 (uint16x8_t a)
{
    return vrev64q (a);
}

/* { dg-final { scan-assembler "vrev64.16"  }  } */
