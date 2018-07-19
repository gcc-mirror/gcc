/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt3 -fdump-tree-optimized" } */

int PopCount (long b) {
    int c = 0;

    while (b) {
	b &= b - 1;
	c++;
    }
    return c;
}

/* { dg-final { scan-tree-dump-times "__builtin_popcount" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "if" 0 "phiopt3" } } */
