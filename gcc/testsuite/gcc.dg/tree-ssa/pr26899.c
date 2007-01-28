/* { dg-options "-fstrict-overflow -fdump-tree-gimple" } */

int foo (int i, int j)
{
  return (i < j + 1) || (j > i - 1);
}

/* { dg-final { scan-tree-dump "j >= i" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */

