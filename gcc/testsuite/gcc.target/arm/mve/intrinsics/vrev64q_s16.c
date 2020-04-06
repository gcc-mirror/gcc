/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a)
{
  return vrev64q_s16 (a);
}

/* { dg-final { scan-assembler "vrev64.16"  }  } */

int16x8_t
foo1 (int16x8_t a)
{
  return vrev64q (a);
}

/* { dg-final { scan-assembler "vrev64.16"  }  } */
