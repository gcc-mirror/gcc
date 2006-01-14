/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct {
	int i;
	int x[2];
	int j;
} a;

int foo(int i)
{
	a.i = 1;
	a.j = 2;
	a.x[i] = 0;
	return a.i + a.j;
}

/* { dg-final { scan-tree-dump "return 3;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

