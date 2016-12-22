/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

#if __SIZEOF_INT__ < 4
#define unsigned __UINT32_TYPE__
#endif

unsigned f(unsigned x)
{
	return (x >> 29) & 32;
}

unsigned g(unsigned x)
{
	return !!(x & 0x80000000) << 5;
}

unsigned j(unsigned x)
{
	return ((x >> 31) & 1) ? 32 : 0;
}

unsigned k(unsigned x)
{
	return (x & 0x80000000) ? 32 : 0;
}

/* { dg-final { scan-tree-dump-not " \\? " "original" } } */
/* { dg-final { scan-assembler-not "sall" { target i?86-*-* x86_64-*-* } } }" */
