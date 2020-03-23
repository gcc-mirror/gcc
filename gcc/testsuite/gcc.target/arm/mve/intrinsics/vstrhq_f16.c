/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float16_t * addr, float16x8_t value)
{
  vstrhq_f16 (addr, value);
}

/* { dg-final { scan-assembler "vstrh.16"  }  } */

void
foo1 (float16_t * addr, float16x8_t value)
{
  vstrhq (addr, value);
}

/* { dg-final { scan-assembler "vstrh.16"  }  } */
