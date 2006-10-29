/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int foo(int x)
{
  return ~(x ^ 4);
}

int bar(int y)
{
  return ~y ^ 4;
}

/* { dg-final { scan-tree-dump-times "x \\^ -5" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "y \\^ -5" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
