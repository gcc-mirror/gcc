/* PR29970*/
/* { dg-do run } */
/* { dg-options "-Wall -O0" } */
/* { dg-require-effective-target alloca } */

int foo(void)
{
	int n = 0;
	return sizeof(*({ n = 10; struct foo { int x[n]; } x; &x; }));
}


int main()
{
	if (sizeof(struct foo { int x[10]; }) != foo())
		__builtin_abort();

	return 0;
}
