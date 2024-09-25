/* { dg-do compile } */
/* { dg-options "-Ofast -march=skylake-avx512 -mfpmath=sse -fno-tree-vectorize -fdump-tree-cddce3-details -fdump-tree-lsplit-optimized -fno-ssa-phiopt" } */
/* Load of d2/d3 is hoisted out, the loop is split, store of d1 and sqrt
   are sunk out of the loop and the loop is elided.  One vsqrtsd with
   memory operand needs a xor to avoid partial dependence.  */
/* Phi-OPT needs to be disabled otherwise, sqrt calls are merged which is better
   but we are testing to make sure the partial register stall for SSE is still avoided
   for sqrts.  */

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

/* { dg-final { scan-tree-dump "optimized: loop split" "lsplit" } } */
/* { dg-final { scan-tree-dump-times "removing loop" 2 "cddce3" } } */
/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
