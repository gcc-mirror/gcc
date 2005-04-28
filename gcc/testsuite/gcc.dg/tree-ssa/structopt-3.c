/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct foo
{
	int a;
	int b;
} temp;
/* We should be able to optimize this to return 11. */
int main(void)
{
	temp.a = 5;
	temp.b = 6;
	return temp.a + temp.b;
}
/* { dg-final { scan-tree-dump-times "return 11" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
