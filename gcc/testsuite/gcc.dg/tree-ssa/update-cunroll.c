/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
int a[8];
int t()
{
	int i;
	for (i = 0; i < 3; i++)
		if (a[i])
			break;
	return i;
}
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
