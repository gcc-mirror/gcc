/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>
#include <stdio.h>
#include <stdlib.h>
#include "../lob.h"

void  __attribute__ ((noinline)) test (int64_t *a, int64_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp64q (n);
      int64x2_t va = vldrdq_gather_offset_z_s64 (a, vcreateq_u64 (0, 8), p);
      vstrdq_scatter_offset_p_s64 (c, vcreateq_u64 (0, 8), va, p);
      c+=2;
      a+=2;
      n-=2;
    }
}

/* { dg-final { scan-assembler-times {\tdlstp.64} 1 } } */
/* { dg-final { scan-assembler-times {\tletp} 1 } } */
/* { dg-final { scan-assembler-not "\tvctp" } } */
/* { dg-final { scan-assembler-not "\tvpst" } } */
/* { dg-final { scan-assembler-not "p0" } } */
