/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fdump-tree-fre1-stats" } */

int foo()
{
  int i = 0;
  do
    {
      i++;
    }
  while (i != 1);
  return i;
}

/* { dg-final { scan-tree-dump "RPO iteration over 3 blocks visited 3 blocks" "fre1" } } */
/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
