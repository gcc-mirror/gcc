/* PR optimization/11864
 * Reporter: Kazumoto Kojima <kkojima@gcc.gnu.org>
 * Summary: [3.3/3.4 regression] miscompiles zero extension and test
 * Description:
 * gcc-3.3/3.4 -O2 for sh target may miscompile the combination of zero extension
 * and test if it's zero.
 *
 * Testcase tweaked by dank@kegel.com.  Not marked as xfail because it's a regression.
 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

int val = 0xff00;

int f(void)
{
	return val;
}

unsigned char a[1];

void foo(void)
{
	a[0] = f() & 255;

	if (!a[0])
		a[0] = f() & 255;

	if (!a[0])
		a[0] = 1 + (f() & 127);
}

int main(int argc, char **argv)
{
	foo();
	if (!a[0])
		abort();

	return 0;
}
