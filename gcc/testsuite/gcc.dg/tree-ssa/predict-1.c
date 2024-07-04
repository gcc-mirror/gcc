/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
int test2();
int
test (int a, int b)
{
	if (__builtin_expect_with_probability (a, 0, 0.4)/__builtin_expect_with_probability (b, 5, 0.2))
		test2();
}
/* { dg-final { scan-tree-dump-times "first match heuristics: 60.00" 1 "profile_estimate"} } */
