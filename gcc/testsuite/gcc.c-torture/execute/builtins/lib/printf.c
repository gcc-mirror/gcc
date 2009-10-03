#include <stdio.h>
#include <stdarg.h>
extern void abort (void);
extern int inside_main;

__attribute__ ((__noinline__))
int
printf (const char *string, ...)
{
  va_list ap;
  int r;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif
  va_start (ap, string);
  r = vprintf (string, ap);
  va_end (ap);
  return r;
}


/* Locking stdio doesn't matter for the purposes of this test.  */
__attribute__ ((__noinline__))
int
printf_unlocked (const char *string, ...)
{
  va_list ap;
  int r;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif
  va_start (ap, string);
  r = vprintf (string, ap);
  va_end (ap);
  return r;
}
