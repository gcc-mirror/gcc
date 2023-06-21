/* PR108423
 * { dg-do compile }
 * { dg-options "-O2 -Wno-int-conversion -Wno-incompatible-pointer-types" }
 */
int f (int n, int (**(*a)(void))[n])
{
	return (*a())[0];
}
int g ()
{
	int m = 3;
	int (*a[m])(void);
	return f(m, &a);
}


