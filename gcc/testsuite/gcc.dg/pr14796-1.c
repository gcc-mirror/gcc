/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int f (int a) {
	return (a << 3) << 6;
}

int g (int b) {
	return (b >> 5) << 5;
}

/* { dg-final { scan-tree-dump "a << 9" "gimple" } } */
/* { dg-final { scan-tree-dump "b & -32" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
