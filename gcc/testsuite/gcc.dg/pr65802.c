/* { dg-do compile } */
/* { dg-options "-O0 -fexceptions" } */
/* { dg-require-effective-target exceptions } */

#include <stdarg.h>

struct S
{
  int (*m_fn1) (void);
} a;

void
fn1 (int d, ...)
{
  va_list c;
  va_start (c, d);

  {
    int *d = va_arg (c, int *);

    int **e = &d;

    a.m_fn1 ();
  }

  a.m_fn1 ();

  va_end (c);
}
