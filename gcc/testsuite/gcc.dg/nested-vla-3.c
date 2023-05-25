/* { dg-do run } */
/* { dg-options "-std=gnu99" } */
/* { dg-require-effective-target trampolines } */


int main()
{
	int n = 1;

	char tmp[2] = { 0 };
	char (*bar(void))[++n] { return &tmp; }

	if (2 != n)
		__builtin_abort();

	if (2 != sizeof(*bar()))
		__builtin_abort();

	n = 1;

	char (*(*bar2)(void))[++n] = bar;

	if (2 != n)
		__builtin_abort();

	if (2 != sizeof(*(*bar2)()))
		__builtin_abort();

}

