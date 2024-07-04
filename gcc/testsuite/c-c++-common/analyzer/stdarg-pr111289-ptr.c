#include <stdarg.h>

static void *
test_1 (const char *fmt, ...)
{
  va_list arg;
  va_start (arg, fmt);

  void *p = va_arg (arg, void *); /* { dg-bogus "-Wanalyzer-va-arg-type-mismatch" } */

  va_end (arg);

  return p;
}

void *
call_test_1 ()
{
  return test_1 ("fmt", "foo");
}

static char *
test_2 (const char *fmt, ...)
{
  va_list arg;
  va_start (arg, fmt);

  char *p = va_arg (arg, char *); /* { dg-bogus "-Wanalyzer-va-arg-type-mismatch" } */

  va_end (arg);

  return p;
}

char *
call_test_2 (void *q)
{
  return test_2 ("fmt", q);
}
