/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct {
	int i;
	int j;
	int x[2];
} a;

int foo(void)
{
	a.i = 1;
	a.j = 0;
	a.x[0] = 0;
	return a.i + a.j;
}

/* { dg-final { scan-tree-dump "return 1;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

