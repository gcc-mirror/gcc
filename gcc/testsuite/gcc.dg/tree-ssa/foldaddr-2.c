/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
struct a{
	int a;
	int b;
} a;
int *
t()
{
	return (int *)&a;
}
/* { dg-final { scan-tree-dump "a.a" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
