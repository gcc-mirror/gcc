/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */

/* This test checks that different field offsets imply
 * that the types can be assumed not to alias
 * and that this is exploited during optimization.  */


struct bar0 { int x; int f[3]; int y; };

[[gnu::noinline,gnu::noipa]]
int test_bar0(struct bar0* a, void* b)
{
	a->x = 1;

	struct bar0 { int x; int f[4]; int y; }* p = b;
	p->x = 2;

	return a->x;
}


/* While these tests check that different structs with different
 * sizes in arrays pointed to by field members can alias,
 * even though the types are incompatible.  */


struct bar1 { int x; int (*f)[3]; };

[[gnu::noinline,gnu::noipa]]
int test_bar1(struct bar1* a, void* b)
{
	a->x = 1;

	struct bar1 { int x; int (*f)[3]; }* p = b;
	p->x = 2;

	return a->x;
}


struct bar2 { int x; int (*f)[3]; };

[[gnu::noinline,gnu::noipa]]
int test_bar2(struct bar2* a, void* b)
{
	a->x = 1;

	struct bar2 { int x; int (*f)[4]; }* p = b;
	p->x = 2;

	return a->x;
}



/* This test checks that different structs with pointers to
 * different compatible arrays types can alias.  */


struct bar3 { int x; int (*f)[3]; };

[[gnu::noinline,gnu::noipa]]
int test_bar3(struct bar3* a, void* b)
{
	a->x = 1;

	struct bar3 { int x; int (*f)[]; }* p = b;
	p->x = 2;

	return a->x;
}




int main()
{
	// control

	struct bar0 z0;

	if (1 != test_bar0(&z0, &z0))
		__builtin_abort();

	// this could be different
	struct bar1 z1;

	if (2 != test_bar1(&z1, &z1))
		__builtin_abort();

	struct bar2 z2;

	if (2 != test_bar2(&z2, &z2))
		__builtin_abort();

	struct bar3 z3;

	if (2 != test_bar3(&z3, &z3))
		__builtin_abort();


	return 0;
}


