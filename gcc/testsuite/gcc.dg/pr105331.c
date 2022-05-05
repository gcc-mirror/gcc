/* PR target/105331 */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

#include <stdarg.h>

int
foo (va_list *va)
{
  return va_arg (*va, double _Complex);	/* { dg-bogus "may be used uninitialized" } */
}
