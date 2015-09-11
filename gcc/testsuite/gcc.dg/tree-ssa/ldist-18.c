/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

void foo (int *p, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    p[i] = 0;
}

/* { dg-final { scan-tree-dump "generated memset zero" "ldist" } } */
