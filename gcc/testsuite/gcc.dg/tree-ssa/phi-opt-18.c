/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo (int a)
{
  int c = 0;
  if (a != 0)
    c = __builtin_popcount (a);
  else
    c = 1;
  return c;
}

/* { dg-final { scan-tree-dump-times "if " 1 "optimized" } } */
