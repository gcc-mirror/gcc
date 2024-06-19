/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>
#include <stdio.h>
#include <stdlib.h>
#include "../lob.h"

void  __attribute__ ((noinline)) test (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c+=4;
      a+=4;
      b+=4;
      n-=4;
    }
}

/* { dg-final { scan-assembler-times {\tdlstp.32} 1 } } */
/* { dg-final { scan-assembler-times {\tletp} 1 } } */
/* { dg-final { scan-assembler-not "\tvctp" } } */
/* { dg-final { scan-assembler-not "\tvpst" } } */
/* { dg-final { scan-assembler-not "p0" } } */
