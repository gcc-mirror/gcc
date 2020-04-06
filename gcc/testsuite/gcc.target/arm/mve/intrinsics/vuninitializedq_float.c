/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O0" } */

#include "arm_mve.h"

void
foo ()
{
  float16x8_t fa;
  float32x4_t fb;
  fa = vuninitializedq_f16 ();
  fb = vuninitializedq_f32 ();
}

/* { dg-final { scan-assembler-times "vstrb.8" 4 } } */
