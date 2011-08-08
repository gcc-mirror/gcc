/* { dg-do compile } */
/* { dg-options "-O1 -foptimize-sibling-calls -fdump-tree-tailr1-details" } */
int
t(int a)
{
	int r;
	if (a)
		r = t(a-1);
	else
		return 0;
	if (r)
		r=r;
	return r;
}
/* { dg-final { scan-tree-dump-times "Eliminated tail recursion" 1 "tailr1"} } */
/* { dg-final { cleanup-tree-dump "tailr1" } } */
