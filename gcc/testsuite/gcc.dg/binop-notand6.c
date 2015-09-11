/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b)
{
  return (a & !a) | (b & (b == 0));
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
