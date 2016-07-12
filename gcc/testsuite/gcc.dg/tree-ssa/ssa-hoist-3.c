/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int test (int a, int b, int c, int g)
{
  int d, e;
  if (a)
    d = b * c;
  else
    d = b - c;
  e = b * c + g;
  return d + e;
}

/* We should hoist and CSE only the multiplication.  */

/* { dg-final { scan-tree-dump-times " \\* " 1 "pre" } } */
/* { dg-final { scan-tree-dump "Insertions: 1" "pre" } } */
