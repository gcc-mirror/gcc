/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -Wzero-as-null-pointer-constant" } */

void foo(void*);

void bar()
{
	enum { E = 0 };
	constexpr _BitInt(4) b0 = 0;
	foo(false);			/* { dg-warning "zero as null pointer constant" } */
	foo(b0);			/* { dg-warning "zero as null pointer constant" } */
	foo(E);				/* { dg-warning "zero as null pointer constant" } */

	void *p = false;		/* { dg-warning "zero as null pointer constant" } */
	void *r = b0;			/* { dg-warning "zero as null pointer constant" } */
	void *t = E;			/* { dg-warning "zero as null pointer constant" } */

	1 ? false : p;			/* { dg-warning "zero as null pointer constant" } */
	1 ? p : false;			/* { dg-warning "zero as null pointer constant" } */
	1 ? b0 : p;			/* { dg-warning "zero as null pointer constant" } */
	1 ? p : b0;			/* { dg-warning "zero as null pointer constant" } */
	1 ? E : p;			/* { dg-warning "zero as null pointer constant" } */
	1 ? p : E;			/* { dg-warning "zero as null pointer constant" } */

	if (p == false);		/* { dg-warning "zero as null pointer constant" } */
	if (false == p);		/* { dg-warning "zero as null pointer constant" } */
	if (p == b0);			/* { dg-warning "zero as null pointer constant" } */
	if (b0 == p);			/* { dg-warning "zero as null pointer constant" } */
	if (p == E);			/* { dg-warning "zero as null pointer constant" } */
	if (E == p);			/* { dg-warning "zero as null pointer constant" } */
}

