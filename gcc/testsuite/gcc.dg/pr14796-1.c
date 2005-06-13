/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int f (int a) {
	return (a << 3) << 6;
}

int g (int b) {
	return (b >> 5) << 5;
}

unsigned long long h (unsigned long long c) {
	return (c << 60) >> 60;
}

int l (int d) {
	return (d << 6) >> 6;
}

/* { dg-final { scan-tree-dump "a << 9" "gimple" } } */
/* { dg-final { scan-tree-dump "b & -32" "gimple" } } */
/* { dg-final { scan-tree-dump "c & 15" "gimple" } } */
/* { dg-final { scan-tree-dump "d << 6" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
