/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

/* x + (x & 1) -> (x + 1) & ~1 */
int
fn1 (int x)
{
	return x + (x & 1);
}
int
fn2 (int x)
{
	return (x & 1) + x;
}
int
fn3 (int x)
{
	return x + (1 & x);
}
int
fn4 (int x)
{
	return (1 & x) + x;
}
unsigned int
fn5 (unsigned int x)
{
	return x + (x & 1);
}
unsigned int
fn6 (unsigned int x)
{
	return (x & 1) + x;
}
unsigned int
fn7 (unsigned int x)
{
	return x + (x % 2);
}
unsigned int
fn8 (unsigned int x)
{
	return (x % 2) + x;
}
unsigned int
fn9 (unsigned int x)
{
	return (1LL & x) + x;
}

/* { dg-final { scan-tree-dump-times "x \\+ 1" 9 "original" } } */
