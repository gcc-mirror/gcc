/* { dg-do "compile" } */
/* { dg-options "-std=c2y -Wpedantic" } */

void f()
{
	_Generic(1, int[*]: 1, default: 0);
	_Generic(1, int(*)[*]: 1, default: 0);
	_Generic(1, int[sizeof(int[*])]: 1, default: 0);	/* { dg-warning "not in a declaration" } */
	_Generic(1, struct { int a[*]; }: 1, default: 0);	/* { dg-warning "variably modified" } */
}

