/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */


/* We check that the incompatible enums as fields lead to
 * incompatible types that can be assumed not to alias
 * and that this is exploited during optimization.  */

struct bar1 { int x; enum A1 { X1 = 1 } f; };

[[gnu::noinline,gnu::noipa]]
int test_bar1(struct bar1* a, void* b)
{
	a->x = 1;

	struct bar1 { int x; enum A1 { X1 = 2 } f; }* p = b;
	p->x = 2;

	return a->x;
}


struct bar2 { int x; enum A2 { X2 = 1 } f; };

[[gnu::noinline,gnu::noipa]]
int test_bar2(struct bar2* a, void* b)
{
	a->x = 1;

	struct bar2 { int x; enum B2 { X2 = 1 } f; }* p = b;
	p->x = 2;

	return a->x;
}



struct bar3 { int x; enum A3 { X3 = 1 } f; };

[[gnu::noinline,gnu::noipa]]
int test_bar3(struct bar3* a, void* b)
{
	a->x = 1;

	struct bar3 { int x; enum A3 { Y3 = 1 } f; }* p = b;
	p->x = 2;

	return a->x;
}


struct bar4 { int x; enum { Z4 = 1 } f; };

[[gnu::noinline,gnu::noipa]]
int test_bar4(struct bar4* a, void* b)
{
	a->x = 1;

	struct bar4 { int x; enum { Z4 = 1 } f; }* p = b;
	p->x = 2;

	return a->x;
}



int main()
{
	struct bar1 z1;

	if (1 != test_bar1(&z1, &z1))
		__builtin_abort();

	struct bar2 z2;

	if (1 != test_bar2(&z2, &z2))
		__builtin_abort();

	struct bar3 z3;

	if (1 != test_bar3(&z3, &z3))
		__builtin_abort();

	struct bar4 z4;
#if 0
	// we used to test this, but this would be incorrect
	// if there is a declaration in another TU cf. PR117490
	if (1 != test_bar4(&z4, &z4))
		__builtin_abort();
#endif
	return 0;
}


