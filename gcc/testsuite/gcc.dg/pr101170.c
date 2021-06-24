/* PR middle-end/101170 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

#include <stdarg.h>

struct S { int a; int b[4]; } s;
va_list ap;
int i;
long long l;

struct S
foo (int x)
{
  struct S a = {};
  do
    if (x)
      return a;
  while (1);
}

__attribute__((noipa)) void
bar (void)
{
  for (; i; i++)
    l |= va_arg (ap, long long) << s.b[i];
  if (l)
    foo (l);
}

void
baz (int v, ...)
{
  va_start (ap, v);
  bar ();
  va_end (ap);
}
