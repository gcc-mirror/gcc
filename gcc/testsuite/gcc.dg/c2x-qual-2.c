/* Tests related to qualifiers and pointers to arrays in C2X, PR98397 */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat" } */

/* test that qualifiers are preserved in tertiary operator for pointers to arrays in C2X */

void f(void)
{
	const int (*u)[1];
	void *v;
	_Static_assert(_Generic(1 ? u : v, const void*: 1, void*: 0), "lost qualifier");	/* { dg-warning "pointer to array loses qualifier in conditional" } */
	_Static_assert(_Generic(1 ? v : u, const void*: 1, void*: 0), "lost qualifier");	/* { dg-warning "pointer to array loses qualifier in conditional" } */
}

/* test that assignment of unqualified to qualified pointers works as expected */

void g(void)
{
	int (*x)[3];
	const int (*p)[3] = x; /* { dg-warning "arrays with different qualifiers"  } */
}

/* test that assignment of qualified void pointers works as expected */

void h(void)
{
	const void* x;
	const int (*p)[3] = x; /* { dg-warning "array with qualifier on the element is not qualified before C2X" } */
}

