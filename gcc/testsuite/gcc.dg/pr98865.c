/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x, int y)
{
  return -(x&1) & y;
}

int bar(int x, int y)
{
  return (x&1) * y;
}

/* { dg-final { scan-tree-dump-times " \\* " 2 "optimized" } } */
