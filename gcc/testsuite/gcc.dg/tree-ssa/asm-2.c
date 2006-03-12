/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#ifdef __hppa__
#define REGISTER "1"
#else
#define REGISTER "0"
#endif

void baz(void)
{
	register int xyzzy asm(REGISTER) = 1;
	asm volatile ("" : : "r"(xyzzy));
}

/* { dg-final { scan-tree-dump-times "asm\[^\\r\\n\]*xyzzy" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
