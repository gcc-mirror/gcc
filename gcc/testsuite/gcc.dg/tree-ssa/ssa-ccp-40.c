/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
  int p = 7;
  int q = p - (x & 5);
  return q & 2;
}

/* { dg-final { scan-tree-dump "return 2;" "optimized" } } */
