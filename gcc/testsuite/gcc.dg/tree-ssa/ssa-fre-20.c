/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int i, j;
int foo(int b)
{
  j = 0;
  if (b)
    goto L2;
L1:
  i = i + 1;
L2:
  i = i + 1;
  if (i == 1)
    goto L1;
  return j;
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
