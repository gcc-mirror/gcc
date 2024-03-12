/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-details-blocks" } */
void
test (int *a, int *b, int n)
{
	for (int i = 0; i < n; i++)
		a[i]+=b[i];
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
