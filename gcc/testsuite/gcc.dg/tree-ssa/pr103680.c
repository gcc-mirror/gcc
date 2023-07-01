/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized-details-blocks -fno-early-inlining" } */
void foo ();
static void
test (int i)
{
	if (__builtin_expect_with_probability (i > 5, 1, 0.6))
		foo ();
}
void
test2(int i)
{
	test (i);
	if (__builtin_expect_with_probability (i > 4, 1, 0.7))
		foo ();
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } }  */
