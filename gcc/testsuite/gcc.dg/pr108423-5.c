/* PR108423
 * { dg-do compile }
 * { dg-options "-O2" }
 */

void f(int n, int (*(*a)(void))[n])
{
    sizeof ((*a)());
}

int (*a(void))[1];

void g(void)
{
	    f(1, a);
}

