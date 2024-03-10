/* PR107557
 * { dg-do compile }
 * { dg-require-effective-target lto }
 * { dg-options "-flto -fsanitize=undefined -fexceptions -Wno-incompatible-pointer-types" }
 */


int c[1][3*2];
int f(int * const m, int (**v)[*m * 2])
{
	return &(c[0][*m]) == &((*v)[0][*m]);
}
int test(int n, int (*(*fn)(void))[n])
{
	return (*fn())[0];
}
int main()
{
	int m = 3;
	int (*d)[3*2] = c;
	int (*fn[m])(void);
	return f(&m, &d) + test(m, &fn);
}


