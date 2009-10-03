/* { dg-lto-do run } */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

char *
myprintf (const char *fmt, ...)
{
  va_list args;
  static char buf[80];

  va_start (args, fmt);
  (void) vsnprintf (buf, sizeof (buf), fmt, args);
  va_end (args);
  return buf;
}

int
main ()
{
  char *s;

  s = myprintf ("%s: %d\n", "foo", 1);
  return strcmp (s, "foo: 1\n") != 0;
}
