/* { dg-do compile } */
/* { dg-options "-O1 -foptimize-sibling-calls -fdump-tree-tailr-details" } */
int
foo (int a)
{
	if (a)
		return a * (2 * (foo (a - 1))) + a + 1;
	else
		return 0;
}
/* { dg-final { scan-tree-dump-times "Eliminated tail recursion" 1 "tailr1"} } */
/* { dg-final { cleanup-tree-dump "tailr\[1-2\]" } } */
