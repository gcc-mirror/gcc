/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-reassoc1" } */

int
ETree_nFactorEntriesInFront (int b, int m)
{
  int nent = b*b + 2*b*m;
  return nent;
}

/* { dg-final { scan-tree-dump-times "\\\*" 2 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
