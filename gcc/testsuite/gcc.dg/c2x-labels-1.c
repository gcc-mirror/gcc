/* Tests for labels before declarations and at ends of compound statements.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -pedantic-errors" } */

int f(int x) 
{ 
	goto b;
	a: int i = 2 * x;
	aa: int u = 0; int v = 0;
           goto c;
	b: goto a;
	{ i *= 3; c: }
	return i + u + v;
        d:
}

int main(void)
{
	if (2 != f(1))
		__builtin_abort();

	return 0;
}
