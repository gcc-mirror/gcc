/* PR optimization/10392
 * Reporter: marcus@mc.pp.se
 * Summary: [3.3/3.4 regression] [SH] optimizer generates faulty array indexing
 * Description:
 * The address calculation of an index operation on an array on the stack 
 * can _under some conditions_ get messed up completely
 *
 * Testcase tweaked by dank@kegel.com
 * Problem only happens with -O2 -m4, so it should only happen on sh4,
 * but what the heck, let's test other architectures, too.
 * Not marked as xfail since it's a regression.
*/
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -m4" { target sh4-*-* } } */
extern void abort (void);
const char *dont_optimize_function_away;

const char *use(const char *str)
{
	dont_optimize_function_away = str;
	if (str[0] != 'v')
		abort();
	if (str[1] < '1' || str[1] > '6')
		abort();
	if (str[2])
		abort();
	return str[2] ? "notused" : "v6";
}

const char *func(char *a, char *b)
{
	char buf[128];
	unsigned char i;
	const char *result;

	char *item[] = {
		"v1",
		"v2",
	};

	buf[0] = 'v';
	buf[1] = '3';
	buf[2] = 0;

	for (i = 0; i < 2; i++) {
		/* bug is: following line passes wild pointer to use() on sh4 -O2 */
		result = use(item[i]);

		use(buf);
		use(a);
		use(b);
		result = use(result);
	}
	return result;
}

int main()
{
	func("v4", "v5");
	return 0;
}

