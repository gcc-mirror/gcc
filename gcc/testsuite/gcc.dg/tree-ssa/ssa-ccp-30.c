/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int
foo (int a)
{
  int b = a & 0xff;
  if (b > 300)
    return 2;
  else
    return 1;
}

/* { dg-final { scan-tree-dump-times "Folding predicate b_.* > 300 to 0" 1 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
