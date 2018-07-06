/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-tree-dce" } */

int foo (int a)
{
  int c = 0;
  if (a != 0)
    {
      __builtin_popcount (a);
      c = 2;
    }
  return c;
}

/* { dg-final { scan-tree-dump-times "if " 1 "optimized" } } */
