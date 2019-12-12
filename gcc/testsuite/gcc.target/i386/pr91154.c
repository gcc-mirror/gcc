/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

void foo (int *dc, int *mc, int *tpdd, int *tpmd, int M)
{
  int sc;
  int k;
  for (k = 1; k <= M; k++)
    {
      dc[k] = dc[k-1] + tpdd[k-1];
      if ((sc = mc[k-1] + tpmd[k-1]) > dc[k]) dc[k] = sc;
      if (dc[k] < -987654321) dc[k] = -987654321;
    }
}

/* We want to convert the loop to SSE since SSE pmaxsd is faster than
   compare + conditional move.  */
/* { dg-final { scan-assembler-not "cmov" } } */
/* { dg-final { scan-assembler-times "pmaxsd" 2 } } */
/* { dg-final { scan-assembler-times "paddd" 2 } } */
