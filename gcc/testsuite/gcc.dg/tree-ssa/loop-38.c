/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details" } */
int a[10];
int b[11];
int q (void);
int
t(int n)
{
   int i;
   int sum = 0;
   for (i=0;i<n;i++)
     if (q())
	sum+=a[i];
     else
	sum+=b[i];
  return sum;
}
/* { dg-final { scan-tree-dump "Loop 1 iterates at most 11 times" "cunrolli" } } */
