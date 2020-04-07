/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O0" } */

#include "arm_mve.h"

float16x8_t fa, faa;
float32x4_t fb, fbb;

void
foo ()
{
  fa = vuninitializedq (faa);
  fb = vuninitializedq (fbb);
}

/* { dg-final { scan-assembler-times "vstrb.8" 6 } } */
