/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int test1 (int x, int y, int z, int weight)
{
  int tmp1 = x * weight;
  int tmp2 = y * weight;
  int tmp3 = (x - y) * weight;
  return tmp1 + (tmp2 + tmp3);
}

int test2 (int x, int y, int z, int weight)
{
  int tmp1 = x * weight;
  int tmp2 = y * weight * weight;
  int tmp3 = z * weight * weight * weight;
  return tmp1 + tmp2 + tmp3;
}

/* There should be one multiplication left in test1 and three in test2.  */

/* { dg-final { scan-tree-dump-times "\\\*" 4 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
