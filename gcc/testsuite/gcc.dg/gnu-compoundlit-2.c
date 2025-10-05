/* { dg-do run } */
/* { dg-options "-std=gnu23 -Wall" } */

[[gnu::noinline,gnu::noipa]]
static bool f(int n)
{
	struct foo { char a[n]; };
	struct foo x = { };

	return 0 == __builtin_memcmp(&x, &(struct foo){ }, sizeof x);
}

int main()
{
	if (!f(7))
		__builtin_abort();

	return 0;
}

