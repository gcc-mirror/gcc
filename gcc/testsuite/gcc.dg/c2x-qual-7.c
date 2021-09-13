/* Tests related to qualifiers and pointers to arrays in C2X, PR98397 */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* test that _Atomic qualifier is not preserved in tertiary operator for pointers to arrays in C2X */

void f(void)
{
	_Atomic void *u;
	void *v;
	_Static_assert(_Generic(1 ? u : v, _Atomic void*: 0, void*: 1), "_Atomic should be removed");
	_Static_assert(_Generic(1 ? v : u, _Atomic void*: 0, void*: 1), "_Atomic should be removed");
}



