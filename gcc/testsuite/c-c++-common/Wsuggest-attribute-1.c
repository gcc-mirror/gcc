/* PR c/98487 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wsuggest-attribute=format" } */

#include <stdarg.h>

[[gnu::__format__(__printf__, 1, 2)]]
void
do_printf(const char * const a0, ...)
{
  va_list ap;
  va_start(ap, a0);
  __builtin_vprintf(a0, ap);
  va_end(ap);
}

[[gnu::__format__(__scanf__, 1, 2)]]
void
do_scanf(const char * const a0, ...)
{
  va_list ap;
  va_start(ap, a0);
  __builtin_vscanf(a0, ap);
  va_end(ap);
}

struct tm;

[[gnu::__format__(__strftime__, 1, 0)]]
void
do_strftime(const char * const a0, struct tm * a1)
{
  char buff[256];
  __builtin_strftime(buff, sizeof(buff), a0, a1);
  __builtin_puts(buff);
}
