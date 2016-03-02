/* PR c/67854 */
/* { dg-do compile } */

#include <stdbool.h>
#include <stdarg.h>

void
foo (va_list ap)
{
  va_arg (ap, bool); /* { dg-warning "is promoted to .int. when passed through" } */
}
