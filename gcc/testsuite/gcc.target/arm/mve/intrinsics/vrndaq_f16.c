/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16x8_t a)
{
  return vrndaq_f16 (a);
}

/* { dg-final { scan-assembler "vrinta.f16"  }  } */
