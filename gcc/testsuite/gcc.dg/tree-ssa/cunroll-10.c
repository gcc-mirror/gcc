/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds -fdump-tree-cunroll-details" } */
int a[3];
int b[4];
main()
{
  int i;
  for (i=0;i<4;i++)
    if (b[i]==2)
     a[i]++;
}
/* { dg-final { scan-tree-dump-times "Forced statement unreachable" 2 "cunroll" } } */
/* { dg-final { cleanup-tree-dump "cunroll" } } */
