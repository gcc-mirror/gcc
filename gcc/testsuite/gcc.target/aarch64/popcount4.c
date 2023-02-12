/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -mgeneral-regs-only" } */

int PopCount (long b) {
    int c = 0;

    while (b) {
	b &= b - 1;
	c++;
    }
    return c;
}

/* { dg-final { scan-tree-dump-times "__builtin_popcount|\\.POPCOUNT" 0 "optimized" } } */
