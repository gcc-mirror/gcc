/* { dg-do run { target int128 } } */

typedef unsigned __int128 u128;

u128 __attribute__((noinline, noclone))
foo(void)
{
	u128 u = -2;
	u %= 0xffffffffffffffffllu;
	return u;
}

int
main()
{
	u128 x = foo();
	if (x != 0xfffffffffffffffellu)
		__builtin_abort();
	return 0;
}
