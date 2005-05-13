/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */
int f (int a, int b) {
	return ~(a ^ -(b + 1));
}

int g (int a, int b) {
	return b ^ a;
}

unsigned int h (unsigned int a, unsigned int b) {
	return ~(-(b + 1) ^ a);
}

/* { dg-final { scan-tree-dump-times "b \\^ a" 3 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
