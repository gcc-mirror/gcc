/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
int test2();
int
test (int a, int b)
{
	if (__builtin_expect_with_probability (a, 0, 0.8)+__builtin_expect_with_probability (b, 5, 0.9) == 5)
		test2();
}
/* Combining two predictions together can not be done precisely, so check that result is DS theory.  */
/* { dg-final { scan-tree-dump-times "combined value predictions heuristics of edge .->.: 72.00" 1 "profile_estimate"} } */
