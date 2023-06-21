/* PR c/109450
 * { dg-do run }
 * { dg-options "-std=gnu99" }
 * */

int bar(int n, struct foo *x)	/* { dg-warning "not be visible" } */
{
	int a = n;
	struct foo { char buf[a++]; }* p = x;
	return n == a;
}

int main()
{
	if (bar(1, 0))
		__builtin_abort();
}

