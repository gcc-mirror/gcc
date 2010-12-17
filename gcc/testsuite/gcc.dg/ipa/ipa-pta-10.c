/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta -fdump-ipa-pta-details" } */

#include <stdarg.h>

static void __attribute__((noinline,noclone))
foo (int i, ...)
{
  va_list ap;
  int *p;
  va_start (ap, i);
  p = va_arg (ap, int *);
  *p = 1;
  va_end (ap);
}
extern void abort (void);
int main()
{
  int i = 0;
  foo (0, &i);
  if (i != 1)
    abort ();
  return 0;
}

/* Verify we properly handle variadic arguments and do not let escape
   stuff through it.  */

/* { dg-final { scan-ipa-dump "ESCAPED = { (ESCAPED )?(NONLOCAL )?}" "pta" } } */
/* { dg-final { cleanup-ipa-dump "pta" } } */
