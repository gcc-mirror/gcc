/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-reassoc1" } */

unsigned int
ETree_nFactorEntriesInFront (unsigned int b, unsigned int m)
{
  unsigned int nent = b*b + 2*b*m;
  return nent;
}

/* { dg-final { scan-tree-dump-times "\\\*" 2 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
