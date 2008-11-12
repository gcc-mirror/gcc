#include <stdio.h>
#include <stdarg.h>
extern void abort (void);
extern int inside_main;

int
(sprintf) (char *buf, const char *fmt, ...)
{
  va_list ap;
  int r;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
  va_start (ap, fmt);
  r = vsprintf (buf, fmt, ap);
  va_end (ap);
  return r;
}


