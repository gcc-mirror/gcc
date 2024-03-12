/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre -fdump-tree-optimized-blocks-details" } */

/* Commutative case.  */

int f(int c, int b, int d)
{
  int r, e;

  if (c)
    r = b + d;
  else
    {
      e = d + b;
      r = e;
    }

  return r;
}

/* { dg-final { scan-tree-dump-times "if " 0 "pre"} } */
/* { dg-final { scan-tree-dump-times "(?n)_.*\\+.*_" 1 "pre"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
