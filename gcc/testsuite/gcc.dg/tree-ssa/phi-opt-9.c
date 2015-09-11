/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int g(int,int);
int h(int);
int f(int t, int c)
{
  int d = 0;
  int e = 0;
  if (t)
    {
      d = h(c);
      e = t;
    }
  else d = 0, e = 0;
  return g(e,d);
}

/* The value e should have been replaced with t and there should be only one PHI. */
/* { dg-final { scan-tree-dump "g .t_\[0-9\]*.D.," "optimized" } } */
/* { dg-final { scan-tree-dump-times "PHI" 1 "optimized" } } */
