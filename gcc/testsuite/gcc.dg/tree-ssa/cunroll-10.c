/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds -fdump-tree-cunroll-details" } */
int a[3];
int b[4];
int
foo (int n)
{
  int i;
  for (i=0;i<n;i++)
    if (b[i]==2)
     a[i]++;
}
/* { dg-final { scan-tree-dump-times "Forced statement unreachable" 2 "cunroll" } } */
