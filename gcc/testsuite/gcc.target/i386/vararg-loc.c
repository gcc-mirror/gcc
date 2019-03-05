/* { dg-do compile } */
/* { dg-options "-g -O0 -fdump-rtl-final" } */

#include <stdarg.h>

int				/* 6.  */
				/* 7.  */
f (int a, ...)			/* 8.  */
				/* 9.  */
{

  int sum = a;

  va_list ap;

  va_start (ap, a);

  sum += va_arg (ap, int);	/* 18.  */

  sum += va_arg (ap, int);	/* 20.  */

  return sum;
}

/* { dg-final { scan-rtl-dump-not "vararg-loc\\.c.:\[6789\] " "final" } } */
/* { dg-final { scan-rtl-dump "vararg-loc\\.c.:18:\[0-9\]+ " "final" } } */
/* { dg-final { scan-rtl-dump "vararg-loc\\.c.:20:\[0-9\]+ " "final" } } */
