/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre -fdump-tree-optimized-details-blocks" } */

/* Different stmt order.  */

int f(int c, int b, int d)
{
  int r, r2, e;

  if (c)
    {
      r = b + d;
      r2 = d - b;
    }
  else
    {
      r2 = d - b;
      e = d + b;
      r = e;
    }

  return r - r2;
}

/* During PRE elimination we should simplify this to return b * 2.  */
/* { dg-final { scan-tree-dump-times "if " 0 "pre" } } */
/* { dg-final { scan-tree-dump "_\[0-9\]+ = b_\[0-9\]+\\(D\\) \\* 2;\[\\r\\n\]\[^\\r\\n\]*return _\[0-9\]+;" "pre" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
