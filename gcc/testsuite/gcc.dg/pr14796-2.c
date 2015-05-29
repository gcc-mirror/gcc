/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-fdump-tree-gimple" } */

int f (int a) {
	return (a << 31) << 6;
}

unsigned int g (unsigned int a) {
	return (a >> 7) >> 25;
}

int h (int b) {
	return (b >> 30) >> 30;
}

long long j (long long c) {
	return (c >> 35) << 35;
}
/* { dg-final { scan-tree-dump-times "= 0" 2 "gimple" } } */
/* { dg-final { scan-tree-dump "b >> 31" "gimple" } } */
/* { dg-final { scan-tree-dump "c & -(34359738368|0x800000000)" "gimple" } } */
