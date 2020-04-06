/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (float16x8_t a)
{
    return vcvtq_u16_f16 (a);
}

/* { dg-final { scan-assembler "vcvt.u16.f16"  }  } */
