/* PR target/84772 */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

#include <stdarg.h>

void
foo (int *x, int y, va_list ap)
{
  __builtin_memset (x, 0, sizeof (int));
  for (int i = 0; i < y; i++)
    va_arg (ap, long double);			/* { dg-bogus "uninitialized" } */  
}
