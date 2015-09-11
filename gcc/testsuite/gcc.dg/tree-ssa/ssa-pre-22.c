/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int foo (int i, int b)
{
  int j = 1;
  if (b)
    j = i;
  return j - i;
}

/* { dg-final { scan-tree-dump "Eliminated: 1" "pre" } } */
