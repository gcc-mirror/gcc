/* PR c/3711
   This testcase ICEd on IA-32 at -O0 and was miscompiled otherwise,
   because std_expand_builtin_va_arg didn't handle variable size types.  */
/* { dg-require-effective-target alloca } */

#include <stdarg.h>

extern void abort (void);
extern void exit (int);

void bar (int c)
{
  static int d = '0';

  if (c != d++)
    abort ();
  if (c < '0' || c > '9')
    abort ();
}

void foo (int size, ...)
{
  struct
  {
    char x[size];
  } d;
  va_list ap;
  int i;

  va_start (ap, size);
  d = va_arg (ap, typeof (d));
  for (i = 0; i < size; i++)
    bar (d.x[i]);
  d = va_arg (ap, typeof (d));
  for (i = 0; i < size; i++)
    bar (d.x[i]);
  va_end (ap);
}

int main (void)
{
  int z = 5;
  struct { char a[z]; } x, y;
          
  x.a[0] = '0';
  x.a[1] = '1';
  x.a[2] = '2';
  x.a[3] = '3';
  x.a[4] = '4';
  y.a[0] = '5';
  y.a[1] = '6';
  y.a[2] = '7';
  y.a[3] = '8';
  y.a[4] = '9';
  foo (z, x, y);
  exit (0);
}
