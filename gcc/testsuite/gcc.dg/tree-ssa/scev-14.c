/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */
int a[100];
void t(unsigned int n)
{
  unsigned int i;
  for (i=0; i<n; i++)
     a[i]++;
}
/* { dg-final { scan-tree-dump "Overflowness wrto loop niter:	No-overflow"  "ivopts" } } */
