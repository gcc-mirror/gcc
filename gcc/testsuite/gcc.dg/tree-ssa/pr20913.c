/* PR tree-optimization/20913
   COPY-PROP did not fold COND_EXPR, blocking some copy propagation
   opportunities.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-copyprop1-details" } */

int
foo (int a, int b, int c, int d)
{
  int x, y;

  b = a;
  if (a == b)
    x = c;
  else
    x = d;

  if (x == c)
    return a;
  else
    return b;
}

/* { dg-final { scan-tree-dump-times "with if \\(1\\)" 2 "copyprop1"} } */
/* { dg-final { cleanup-tree-dump "copyprop1" } } */
