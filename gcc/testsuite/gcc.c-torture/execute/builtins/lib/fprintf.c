#include <stdio.h>
#include <stdarg.h>
extern int inside_main;

int
fprintf (FILE *fp, const char *string, ...)
{
  va_list ap;
  int r;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif
  va_start (ap, string);
  r = vfprintf (fp, string, ap);
  va_end (ap);
  return r;
}
