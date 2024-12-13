/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -save-temps -mtune=cortex-m55" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>
#include <stdio.h>
#include <stdlib.h>
#include "../lob.h"

void  __attribute__ ((noinline)) test (int8_t *a, int8_t *b, int8_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp8q (n);
      int8x16_t va = vldrbq_z_s8 (a, p);
      int8x16_t vb = vldrbq_z_s8 (b, p);
      int8x16_t vc = vaddq_x_s8 (va, vb, p);
      vstrbq_p_s8 (c, vc, p);
      c+=16;
      a+=16;
      b+=16;
      n-=16;
    }
}


/* { dg-final { scan-assembler-times {\tdlstp.8} 1 } } */
/* { dg-final { scan-assembler-times {\tletp} 1 } } */
/* { dg-final { scan-assembler-not "\tvctp" } } */
/* { dg-final { scan-assembler-not "\tvpst" } } */
/* { dg-final { scan-assembler-not "p0" } } */
