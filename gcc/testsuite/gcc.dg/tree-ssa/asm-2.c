/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#if defined(__hppa__) || defined(__v850__)
#define REGISTER "1"
#else
#ifdef __moxie__
#define REGISTER "2"
#else
#define REGISTER "0"
#endif
#endif

void baz(void)
{
	register int xyzzy asm(REGISTER) = 1;
	asm volatile ("" : : "r"(xyzzy));
}

/* { dg-final { scan-tree-dump-times "asm\[^\\r\\n\]*xyzzy" 1 "optimized" } } */
