/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
foo (int a[], int b[], int i)
{
  a[i] = b[i] + 2;
  i++;
  a[i] = b[i] + 2;
  i++;
  a[i] = b[i] + 2;
  i++;
  a[i] = b[i] + 2;
  i++;
  return i;
}

/* { dg-final { scan-tree-dump-times "\\* 4" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 4" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 8" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 12" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
