/* { dg-do compile } */

#include <stdarg.h>

va_list v;
volatile int i;

void foo()
{
  i = va_arg(v, char); /* { dg-error "is promoted to|so you should" "char" } */
  i = va_arg(v, short); /* { dg-error "is promoted to" "short" } */
  i = va_arg(v, float); /* { dg-error "is promoted to" "float" } */
}
