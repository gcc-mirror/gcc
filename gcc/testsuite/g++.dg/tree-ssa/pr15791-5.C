/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int foo(int i, int j)
{
	char g[16];
	if (&g[i] == &g[j])
		return 1;
	return 0;
}

/* { dg-final { scan-tree-dump-times "i == j" 1 "gimple" } } */

