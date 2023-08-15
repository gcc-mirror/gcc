/* { dg-do run }
 * { dg-options "-std=c23 -O2" }
 */


/* These tests check that redefinitions of tagged
   types can alias the original definitions.  */

struct foo { int x; };

int test_foo(struct foo* a, void* b)
{
	a->x = 1;

	struct foo { int x; }* p = b;
	p->x = 2;

	return a->x;
}


enum bar { A = 1, B = 3 };

int test_bar(enum bar* a, void* b)
{
	*a = A;

	enum bar { A = 1, B = 3 }* p = b;
	*p = B;

	return *a;
}


int main()
{
	struct foo y;

	if (2 != test_foo(&y, &y))
		__builtin_abort();

	enum bar z;

	if (B != test_bar(&z, &z))
		__builtin_abort();

	return 0;
}

