/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-details-blocks" } */
int a[99];
void test()
{
	for (int i = 0; i < 99; i++)
		a[i]++;
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized"} } */
