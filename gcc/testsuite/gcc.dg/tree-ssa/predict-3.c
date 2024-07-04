/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
int test2();
int
test (int p, int a, int b, int c)
{
	int d;
	if (p)
	   d = __builtin_expect_with_probability (a, 0, 0.8) * b;
	else
	   d = __builtin_expect_with_probability (a, 0, 0.8) * c;
	if (d)
		test2();
}
/* { dg-final { scan-tree-dump-times "first match heuristics: 20.00" 1 "profile_estimate"} } */
