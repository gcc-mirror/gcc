/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a)
{
    return vrev64q_u32 (a);
}

/* { dg-final { scan-assembler "vrev64.32"  }  } */

uint32x4_t
foo1 (uint32x4_t a)
{
    return vrev64q (a);
}

/* { dg-final { scan-assembler "vrev64.32"  }  } */
