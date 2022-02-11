/* { dg-do compile } */
/* { dg-options "-Ofast -march=skylake-avx512 -mfpmath=sse" } */


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

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
