/* { dg-do compile } */
/* { dg-options "-std=c90" } */

#include <stdarg.h>


int f(int n, ...)
{
	va_list ap;
	va_start(ap, n);
	_Static_assert(5 == sizeof(va_arg(ap, char[5])));
	va_arg(ap, int[n]);			/* { dg-error "array type" } */
	int * a = va_arg(ap, int[3]);		/* { dg-error "invalid use of non-lvalue array" } */
}

