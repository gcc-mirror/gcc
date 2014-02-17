/* Test for cross x86_64<->w64 abi va_list calls.  */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -fno-builtin" } */

#include <stdarg.h>

extern __SIZE_TYPE__ __attribute__ ((sysv_abi)) strlen (const char *);
extern int __attribute__ ((sysv_abi)) sprintf (char *, const char *, ...);

static void
vdo_cpy (char *s, va_list argp)
{
  __SIZE_TYPE__ len;
  char *r = s;
  char *e;
  *r = 0;
  for (;;) {
    e = va_arg (argp, char *);
    if (*e == 0) break;
    sprintf (r,"%s", e);
    r += strlen (r);
  }
}

void
do_cpy (char *s, ...)
{
  va_list argp;
  va_start (argp, s);
  vdo_cpy (s, argp);
  va_end (argp);
}
