/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-ch2-blocks-details -fdump-tree-optimized-blocks-details" } */
void foo ();
void test()
{
	for (int i = 0; i < 10; i++)
		foo ();
}
/* We should figure out that after header dulication loop iterates 9 times.  */
/* { dg-final { scan-tree-dump "90.00" "ch2"} } */
/* { dg-final { scan-tree-dump "10.00" "ch2"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "ch2"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
