/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int foo(int i)
{
  return i - -1;
}

/* { dg-final { scan-tree-dump "i \\+ 1" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
