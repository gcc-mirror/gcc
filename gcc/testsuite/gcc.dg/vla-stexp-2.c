/* PR101838 */
/* { dg-do run } */
/* { dg-options "-std=gnu17 -Wpedantic -O0" } */


int bar0(
	int (*a)[*],
	int (*b)[sizeof(*a)]
);


int bar(
	struct f { 		/* { dg-warning "will not be visible outside of this definition" } */
		int a[*]; } v,	/* { dg-warning "variably modified type" } */
	int (*b)[sizeof(struct f)]	// should not warn about zero size
);

int foo(void)
{
	int n = 0;
	return sizeof(typeof(*({ n = 10; struct foo { 	/* { dg-warning "braced-groups" } */
					int x[n]; 	/* { dg-warning "variably modified type" } */
	} x; &x; })));
}


int main()
{
	if (sizeof(struct foo { int x[10]; }) != foo())
		__builtin_abort();

	return 0;
}
