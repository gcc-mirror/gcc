/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define REGISTER "0"

void baz(void)
{
	register int xyzzy asm(REGISTER) = 1;
	asm volatile ("" : : "r"(xyzzy));
}

/* { dg-final { scan-tree-dump-times "asm\[^\\r\\n\]*xyzzy" 1 "optimized" } } */
