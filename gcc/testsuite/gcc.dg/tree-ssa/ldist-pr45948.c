/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

extern void bar(int);

void
foo (int i, int n)
{
  int a[30];
  int b[30];
  for (; i < n; i++)
    a[i] = b[i] = 0;

  while (1)
    if (b[0])
      bar (a[i - 1]);
}

/* We should apply loop distribution and generate 2 memset (0).  */

/* { dg-final { scan-tree-dump "distributed: split to 2" "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 2 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
