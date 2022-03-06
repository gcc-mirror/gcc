/* PR target/104380 */
/* This test needs runtime that provides __*_chk functions.  */
/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -std=c99" } */

#define FORTIFY_SOURCE 2
#include <stdio.h>
#include <stdarg.h>

static char buf[4096];
static char gfmt[] = "%Lg";

static int __attribute__ ((noipa))
foo (char *str, const char *fmt, ...)
{
  int ret;
  va_list ap;
  va_start (ap, fmt);
  ret = vsnprintf (str, 4096, fmt, ap);
  va_end (ap);
  return ret;
}

int
main ()
{
  long double dval = 128.0L;
  int ret = foo (buf, gfmt, dval);
  if (ret != 3 || __builtin_strcmp (buf, "128") != 0)
    __builtin_abort ();
  return 0;
}
