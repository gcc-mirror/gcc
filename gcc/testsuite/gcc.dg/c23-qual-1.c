/* Tests related to qualifiers and pointers to arrays in C23, PR98397 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* test that qualifiers are preserved in tertiary operator for pointers to arrays in C23 */

void f(void)
{
	const int (*u)[1];
	void *v;
	_Static_assert(_Generic(1 ? u : v, const void*: 1, void*: 0), "lost qualifier");
	_Static_assert(_Generic(1 ? v : u, const void*: 1, void*: 0), "lost qualifier");
}

/* test that assignment of unqualified to qualified pointers works as expected */

void g(void)
{
	int (*x)[3];
	const int (*p)[3] = x;
}

/* test that assignment of qualified void pointers works as expected */

void h(void)
{
	const void* x;
	const int (*p)[3] = x;
}

