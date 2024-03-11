/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunroll-blocks-details -fno-tree-vectorize" } */
struct a {int a[100];};
void
t(struct a *a)
{
  for (int i=0;i<5 && a->a[i];i++)
    a->a[i]++;
}
/* { dg-final { scan-tree-dump-times "loop with 4 iterations completely unrolled" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Loop 1 iterates 4 times" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Last iteration exit edge was proved true" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Exit condition of peeled iterations was eliminated" 1 "cunroll" } } */
