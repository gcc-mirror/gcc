/* { dg-do compile }
 * { dg-options "-std=c23 -O2" }
 */

/* The structs are incompatible so can be assumed not to
 * alias, but this is not exploited.  So do not check for 
 * this below but check the error about incompatibility.  */

struct bar { int x; int f[]; };

int test_bar3(struct bar* a, void* b)
{
	a->x = 1;

	struct bar { int x; int f[1]; }* p = b;
	struct bar* q = a;			/* { dg-error "incompatible" } */
	p->x = 2;

	return a->x;
}


int main()
{
	struct bar z;

	// allow both results here
	int r = test_bar3(&z, &z);

	// UB but could be expected to return 1 with optimization
	// exploiting the UB (not done at time of writing) or 2

	return r;
}


