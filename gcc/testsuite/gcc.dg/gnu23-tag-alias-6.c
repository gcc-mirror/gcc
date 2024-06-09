/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */



/* Here we check that struct with a variable size (GNU extension)
 * can alias a struct with a flexible array member or a struct with a
 * fixed size array as last element.  */

struct bar { int x; int f[]; };

[[gnu::noinline,gnu::noipa]]
int test_bar4(struct bar* a, void* b)
{
	a->x = 1;

	int n = 3;
	struct bar { int x; int f[n]; }* p = b;
	struct bar* q = a;
	p->x = 2;

	return a->x;
}


struct foo { int x; int f[3]; };


[[gnu::noinline,gnu::noipa]]
int test_foo1(struct foo* a, void* b)
{
	a->x = 1;

	int n = 3;
	struct foo { int x; int f[n]; }* p = b;
	struct foo* q = a;
	p->x = 2;

	return a->x;
}



int main()
{
	struct bar z;

	if (2 != test_bar4(&z, &z))
		__builtin_abort();

	struct foo y;

	if (2 != test_foo1(&y, &y))
		__builtin_abort();

	return 0;
}


