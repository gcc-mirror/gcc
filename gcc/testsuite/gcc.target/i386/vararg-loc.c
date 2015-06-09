/* { dg-do compile } */
/* { dg-options "-g -O0" } */

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

/* { dg-final { scan-assembler-not "\\.loc 1 \[6789\] 0" } } */
/* { dg-final { scan-assembler-times "\\.loc 1 18 0" 1 } } */
/* { dg-final { scan-assembler-times "\\.loc 1 20 0" 1 } } */
