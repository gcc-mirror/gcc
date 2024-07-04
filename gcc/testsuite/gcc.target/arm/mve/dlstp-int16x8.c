/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>
#include <stdio.h>
#include <stdlib.h>
#include "../lob.h"

void  __attribute__ ((noinline)) test (int16_t *a, int16_t *b, int16_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      int16x8_t va = vldrhq_z_s16 (a, p);
      int16x8_t vb = vldrhq_z_s16 (b, p);
      int16x8_t vc = vaddq_x_s16 (va, vb, p);
      vstrhq_p_s16 (c, vc, p);
      c+=8;
      a+=8;
      b+=8;
      n-=8;
    }
}

/* { dg-final { scan-assembler-times {\tdlstp.16} 1 } } */
/* { dg-final { scan-assembler-times {\tletp} 1 } } */
/* { dg-final { scan-assembler-not "\tvctp" } } */
/* { dg-final { scan-assembler-not "\tvpst" } } */
/* { dg-final { scan-assembler-not "p0" } } */
