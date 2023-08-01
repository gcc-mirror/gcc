/* { dg-do compile } */
/* { dg-options "-Ofast -march=skylake-avx512 -mfpmath=sse -fno-tree-vectorize" } */
/* Load of d2/d3 is hoisted out, vrndscalesd will reuse loades register to avoid partial dependence.  */

#include<math.h>

extern double d1, d2, d3;
void
foo (int n, int k)
{
  for (int i = 0; i != n; i++)
    if(i < k)
      d1 = sqrt (d2);
    else
      d1 = sqrt (d3);
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 0 } } */
