/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */


/* This test fails when the bitfield is not marked
   nonaddressable in the composite type.  */

struct foo { int x :3; } x;

[[gnu::noinline,gnu::noipa]]
int test_foo1(struct foo* a, void* b)
{
	a->x = 1;

	struct foo { int x :3; } y;
	typeof(*(1 ? &x : &y)) *z = b;

	z->x = 2;

	return a->x;
}

int main()
{
	struct foo y;

	if (2 != test_foo1(&y, &y))
		__builtin_abort();

	return 0;
}


