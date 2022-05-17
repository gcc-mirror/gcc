/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-flto" } */
/* { dg-additional-sources stdarg-lto-1-b.c } */

#include <stdarg.h>
#include "stdarg-lto-1.h"

/* Type mismatch: expect const char *, but passed an int.  */

void
called_by_test_type_mismatch_1 (int placeholder, ...)
{
  const char *str;
  
  va_list ap;
  va_start (ap, placeholder);

  str = va_arg (ap, const char *); /* { dg-warning "'va_arg' expected '\[^\n\r\]*' but received 'int' for variadic argument 1 of 'ap'" } */

  va_end (ap);
}

int main() { return 0; }
