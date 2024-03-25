/* { dg-do run }
 * { dg-options "-std=c23 -O2" }
 */


/* These tests check that a composite type for a struct
 * can alias the original definition.  */

struct foo { int (*y)[]; int x; } s;

int test_foo(struct foo* a, void* b)
{
	a->x = 1;

	struct foo { int (*y)[1]; int x; } t;
	typeof(*(1 ? &s: &t)) *p = b;
	p->x = 2;

	return a->x;
}


int main()
{
	struct foo y;

	if (2 != test_foo(&y, &y))
		__builtin_abort();

	return 0;
}

