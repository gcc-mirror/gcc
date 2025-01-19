/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */

/* This used to check that an incompatible definition of
 * a tagged type without tag can be assumed not to alias.  
 * and that this is exploited during optimization.  
 * Because PR117490 we now check the opposite. */



typedef struct { int x; } foo_t;

[[gnu::noinline,gnu::noipa]]
int test_foo(foo_t* a, void* b)
{
	a->x = 1;

	struct { int x; }* p = b;
	p->x = 2;

	return a->x;
}


int main()
{
	foo_t y;

	if (2 != test_foo(&y, &y))
		__builtin_abort();

	return 0;
}


