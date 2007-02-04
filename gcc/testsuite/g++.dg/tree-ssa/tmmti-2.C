/* { dg-do compile } */
/* { dg-options { -O -fdump-tree-optimized } } */

int a[4][8];

int foo(int i)
{
	return *(&a[0][0] + i*8); // a[i][0]
}

struct Foo { double x, y; };

Foo b[4];

double bar(int i)
{
	return *(&b[0].x + i*2); // b[i].x
}

/* { dg-final { scan-tree-dump "a\\\[.*i.*\\\]\\\[0\\\]" "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "b\\\[.*i.*\\\].x" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
