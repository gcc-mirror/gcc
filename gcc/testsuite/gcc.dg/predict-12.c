/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
int *a,n,m;
void test(void);
void
t(void)
{
  int i,j;
  for (i=0;i<n;i++)
    if (a[i])
      for (j=0;j<m;j++)
	if (a[i+j])
	  t();
}
/* { dg-final { scan-tree-dump-times "loop guard with recursion" 1 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "loop exit with recursion" 2 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "recursive call" 1 "profile_estimate"} } */
