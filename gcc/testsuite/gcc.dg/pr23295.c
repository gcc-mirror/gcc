/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int foo(int i)
{
  return -i - 5;
}

/* { dg-final { scan-tree-dump "-5 - i" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
