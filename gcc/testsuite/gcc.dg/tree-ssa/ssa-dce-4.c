/* { dg-do compile } */
/* { dg-options "-O -fno-tree-fre -fdump-tree-cddce1" } */

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

/* Verify DCE removes all accesses to a but the last store and the
   read from a[2].  */
/* { dg-final { scan-tree-dump-times "a\\\[\[^\n\]\\\]" 2 "cddce1" } } */
