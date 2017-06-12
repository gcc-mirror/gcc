/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details -fdump-tree-optimized" } */

extern void bar(int);

void
foo (unsigned i, unsigned n)
{
  int a[30];
  int b[30];
  if (n == 0)
    return;
  for (i=0; i < n; i++)
    a[i] = b[i] = 0;

  while (1)
    if (b[0])
      bar (a[i - 1]);
}

/* We should apply loop distribution and generate 1 memset (0).  PRE optimizes
   away a[] completely.  */

/* { dg-final { scan-tree-dump "distributed: split to 0 loops and 1 library calls" "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 1 "ldist" } } */
/* { dg-final { scan-tree-dump-times "int a" 0 "optimized" } } */
