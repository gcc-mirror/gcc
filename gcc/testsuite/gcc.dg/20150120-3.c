/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

/* x | ~(x | y) -> x | ~y */
int fn1 (int x, int y)
{
	return x | ~(x | y);
}
int fn2 (int x, int y)
{
	return ~(x | y) | x;
}
int fn3 (int x, int y)
{
	return x | ~(y | x);
}
int fn4 (int x, int y)
{
	return ~(y | x) | x;
}
int fn5 (int z)
{
	return z | ~(z | 3);
}
int fn6 (int z)
{
	return ~(z | 3) | z;
}


/* { dg-final { scan-tree-dump-times "~y \\| x" 4 "original" } } */
/* { dg-final { scan-tree-dump-times "z \\| -4" 2 "original" } } */
