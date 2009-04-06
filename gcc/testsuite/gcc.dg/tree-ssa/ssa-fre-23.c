/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre" } */

int f(int t, int a, int b)
{
  int c, d;
  if (t)
    {
      c = a;
      d = a;
    }
  else
    {
      c = b;
      d = b;
    }
  return c+d;
}

/* { dg-final { scan-tree-dump-times "PHI" 1 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
