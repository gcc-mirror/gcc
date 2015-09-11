/* { dg-do run } */
/* { dg-options "-O3" } */

short c = 0;

int __attribute__ ((noinline)) f(void)
{
	int d = 5;
	signed char e = (c != 1) * -2;
	int a = (unsigned short)e > d;

	return a;
}

int main(void)
{
	if (!f())
		__builtin_abort();

	return 0;
}
