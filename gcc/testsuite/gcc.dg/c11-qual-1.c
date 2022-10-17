/* Test that qualifiers are lost in tertiary operator for pointers to arrays before C2X, PR98397 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-discarded-array-qualifiers" } */

void foo(void)
{
	const int (*u)[1];
	void *v;
	_Static_assert(_Generic(1 ? u : v, const void*: 0, void*: 1), "qualifier not lost");
	_Static_assert(_Generic(1 ? v : u, const void*: 0, void*: 1), "qualifier not lost");
}
