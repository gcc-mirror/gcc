/* { dg-do run } */
/* { dg-options "-std=gnu23" } */

#include <stdarg.h>

int f(int n, ...)
{
	__label__ b, d;
	va_list ap;
	va_start(ap, n);
	_Static_assert(5 == sizeof(va_arg(ap, char[5])));	/* { dg-warning "array type" } */
	void g(void) { n++; goto b; }
	int *a = va_arg((g(), ap), int[n]);	/* { dg-warning "array type" } */
b:
	void h(void) { n++; goto d; }
	typeof(va_arg(ap, int[(h(), n)])) c;	/* { dg-warning "array type" } */
d:
	return n;
}

int main()
{
	if (9 != f(7))
		__builtin_abort();
	return 0;
}

