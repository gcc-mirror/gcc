/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

int
f (int c, int b, int d)
{
  int r, e;

  if (c)
    r = b + d;
  else
    {
      e = b + d;
      r = e;
    }

  return r;
}

/* { dg-final { scan-tree-dump-times "if " 0 "pre"} } */
/* { dg-final { scan-tree-dump-times "_.*\\\+.*_" 1 "pre"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
