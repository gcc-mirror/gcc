/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo (int a)
{
  int c = 0;
  if (a != 0)
    c = __builtin_popcount (a);
  return c;
}

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
