/* { dg-do run } */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -fno-builtin" } */

#include <stdarg.h>

extern void __attribute__ ((sysv_abi)) abort (void);

char *a = "1";
char *b = "2";

static void __attribute__((noinline,noclone))
do_cpy2 (va_list argp)
{
  char *e;

  e = va_arg (argp, char *);
  e = va_arg (argp, char *);
  if (e != b)
    abort ();
}

void __attribute__((noinline,noclone))
do_cpy (int dummy, ...)
{
  va_list argp;

  va_start (argp, dummy);

  do_cpy2 (argp);

  va_end (argp);
}

int __attribute__ ((sysv_abi))
main ()
{
  do_cpy (0, a, b);

  return 0;
}
