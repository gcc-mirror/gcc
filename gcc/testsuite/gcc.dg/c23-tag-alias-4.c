/* { dg-do run }
 * { dg-options "-std=c23 -O2" }
 */


/* Here we check that structs with flexible array
 * members can alias a compatible redefinition.  */

struct bar { int x; int f[]; };

int test_bar1(struct bar* a, void* b)
{
	a->x = 1;

	struct bar { int x; int f[]; }* p = b;
	struct bar* q = a;
	p->x = 2;

	return a->x;
}

int main()
{
	struct bar z;

	if (2 != test_bar1(&z, &z))
        	__builtin_abort();

	return 0;
}


