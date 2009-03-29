/* Test for VLA size evaluation in va_arg.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

#include <stdarg.h>

extern void exit (int);
extern void abort (void);

int a[10];
int i = 9;

void
f (int n, ...)
{
  va_list ap;
  void *p;
  va_start (ap, n);
  p = va_arg (ap, typeof (int (*)[++i]));
  if (p != a)
    abort ();
  if (i != n)
    abort ();
  va_end (ap);
}

int
main (void)
{
  f (10, &a);
  exit (0);
}
