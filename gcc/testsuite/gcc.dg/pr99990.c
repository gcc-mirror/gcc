/* PR c/99990 */
/* { dg-do compile } */
/* { dg-options "" } */

#include <stdarg.h>

void
foo ()
{
  va_arg (0, long);	/* { dg-error "first argument to 'va_arg' not of type 'va_list'" } */
  void *b[] = 0;	/* { dg-error "invalid initializer" } */
}
