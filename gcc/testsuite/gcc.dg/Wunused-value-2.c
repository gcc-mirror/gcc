/* Test -Wunused-value.  Bug 30729.  */
/* { dg-do compile } */
/* { dg-options "-Wunused-value" } */
/* Make sure va_arg does not cause a value computed is not used warning
   because it has side effects.   */
#include <stdarg.h>

int f(int t, ...)
{
  va_list a;
  va_start (a, t);
  va_arg(a, int);/* { dg-bogus "value computed is not used" } */
  int t1 = va_arg(a, int);
  va_end(a);
  return t1;
}


