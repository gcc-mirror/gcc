/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int test3 (int x, int y, int z, int weight, int w1, int w2, int w3)
{
  int wtmp1 = w1 * weight;
  int wtmp2 = w2 * weight;
  int wtmp3 = w3 * weight;
  int tmp1 = x * wtmp1;
  int tmp2 = y * wtmp2;
  int tmp3 = z * wtmp3;
  return tmp1 + tmp2 + tmp3;
}

/* The multiplication with weight should be un-distributed.
   ???  This pattern is not recognized currently.  */

/* { dg-final { scan-tree-dump-times "\\\*" 4 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
