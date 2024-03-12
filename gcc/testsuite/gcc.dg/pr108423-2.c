/* PR108423
 * { dg-do compile }
 * { dg-options "-O2" }
 */

void f(int n, int (*a(void))[n])
{
	(a())[0];
}

void g(void)
{
	int (*a(void))[1];
	f(1, a);
}

