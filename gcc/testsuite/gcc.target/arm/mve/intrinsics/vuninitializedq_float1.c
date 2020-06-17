/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t fa, faa;
float32x4_t fb, fbb;

void
foo ()
{
  fa = vuninitializedq (faa);
  fb = vuninitializedq (fbb);
}

/* { dg-final { scan-assembler-times "vstrh.16" 1 } } */
/* { dg-final { scan-assembler-times "vstrw.32" 1 } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
