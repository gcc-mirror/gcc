/* { dg-do run }
 * { dg-options "-std=gnu23 -O2" }
 */

/* Check that structs with flexible array member can alias.  */

struct bar { int x; int f[]; };

[[gnu::noinline,gnu::noipa]]
int test_bar2(struct bar* a, void* b)
{
	a->x = 1;

	struct bar { int x; int f[0]; }* p = b;
	struct bar* q = a;
	p->x = 2;

	return a->x;
}



int main()
{
	struct bar z;

	if (2 != test_bar2(&z, &z))
		__builtin_abort();

	return 0;
}


