/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt-details-alias" } */
/* Testcase to make sure restrict works correctly by zeroing out clique/base. */

int
f (int c, int * __restrict fp, int *q)
{
  int r;
  if (c)
    r = *fp;
  else
    r = *q;
  return r + 1;
}
/* { dg-final { scan-tree-dump-times "changed to factor out load from" 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "clique " "phiopt2" } } */
