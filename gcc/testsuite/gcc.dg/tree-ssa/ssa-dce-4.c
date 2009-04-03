/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int foo(int b)
{
  int a[128];
  a[b] = 1;
  if (b)
    {
      b = 2;
      a[2] = 0;
    }
  a[2] = 3;
  return a[2] + b;
}

/* { dg-final { scan-tree-dump-times "a\\\[\[^\n\]\\\]" 2 "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
