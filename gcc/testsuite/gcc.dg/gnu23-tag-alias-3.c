/* { dg-do run { target lto } }
 * { dg-options "-std=gnu23 -flto -O2" }
 */

/* These tests check that incompatible definitions of
   tagged types can be assumed not to alias and that
   this is exploited during optimization with LTO.  */

struct foo { int x; };

[[gnu::noinline,gnu::noipa]]
int test_foo1(struct foo* a, void* b)
{
	a->x = 1;

	struct foo { int x; int y; }* p = b;
	p->x = 2;

	return a->x;
}

[[gnu::noinline,gnu::noipa]]
int test_foo2(struct foo* a, void* b)
{
	a->x = 1;

	struct fox { int x; }* p = b;
	p->x = 2;

	return a->x;
}


/* While these tests check that incompatible definitions
 * of enums can alias.  */

enum bar { A = 1, B = 3, C = 5, D = 9 };

[[gnu::noinline,gnu::noipa]]
int test_bar1(enum bar* a, void* b)
{
	*a = A;

	enum bar { A = 1, B = 3, C = 6, D = 9 }* p = b;
	*p = B;

	return *a;
}

[[gnu::noinline,gnu::noipa]]
int test_bar2(enum bar* a, void* b)
{
	*a = A;

	enum baX { A = 1, B = 3, C = 5, D = 9 }* p = b;
	*p = B;

	return *a;
}


int main()
{
	struct foo y;

	if (1 != test_foo1(&y, &y))
		__builtin_abort();

	if (1 != test_foo2(&y, &y))
		__builtin_abort();

	enum bar z;

	if (B != test_bar1(&z, &z))
		__builtin_abort();

	if (B != test_bar2(&z, &z))
		__builtin_abort();

	return 0;
}


