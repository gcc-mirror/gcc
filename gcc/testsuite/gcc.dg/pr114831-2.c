/* { dg-do run } 
 * { dg-options "-std=c23" } */

int foo(int n)
{
	int a[1][n];
	typeof((n++,a)) b2;
	return n;
}

int main()
{
	if (2 != foo(1))
		__builtin_abort();
}

