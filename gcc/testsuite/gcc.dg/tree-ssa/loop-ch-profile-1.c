/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-ch2-blocks-details -fdump-tree-optimized" } */
void foo ();
void test(int v, int q)
{
	for (int i = 0; i < 10 && v/q; i++)
		foo ();
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "ch2"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
