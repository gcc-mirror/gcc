/* PR tree-optimization/18828 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdarg.h>

extern void abort (void);

void foo (int x, ...)
{
  va_list ap;
  if (x != 21)
    abort ();
  va_start (ap, x);
  va_end (ap);
}

void bar (int x, ...)
{
  va_list ap;
  x++;
  va_start (ap, x);
  va_end (ap);
}

void baz (int x, ...)
{
  va_list ap;
  x = 0;
  va_start (ap, x);
  va_end (ap);
}
