/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct {
	int i;
	struct {
		int x[2];
	} b;
} a;

int foo(void)
{
	a.i = 1;
	a.b.x[0] = 0;
	a.b.x[1] = 1;
	return a.i + a.b.x[0];
}

/* { dg-final { scan-tree-dump "return 1;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

