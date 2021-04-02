/* PR c/97981 */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-final { scan-tree-dump-times "atomic_load" 2 "original" } } */


void f(void)
{
	volatile _Atomic int x;
	x;
	volatile _Atomic double a;
	double b;
	b = a;
}

