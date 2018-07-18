/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

int f(int x)
{
	return (x << 2) & 128;
}

int g(int x)
{
	return !!(x & 32) << 7;
}

int h(int x)
{
	return ((x >> 5) & 1) << 7;
}

int i(int x)
{
	return (x & 32) >> 5 << 7;
}

int j(int x)
{
	return ((x >> 5) & 1) ? 128 : 0;
}

int k(int x)
{
	return (x & 32) ? 128 : 0;
}

/* { dg-final { scan-tree-dump-not " \\? " "original" } } */
/* { dg-final { scan-assembler-not "sarl" { target i?86-*-* x86_64-*-* } } }" */
