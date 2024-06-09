/* { dg-do run } 
 * { dg-options "-std=gnu23" } */

int foo(int n)
{
	int (*a(void))[n] { return 0; };
	typeof((n++,a)) b2;
	return n;
}

int main()
{
	if (2 != foo(1))
		__builtin_abort();
}

