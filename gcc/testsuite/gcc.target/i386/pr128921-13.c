/* PR target/128921 */
/* { dg-do run { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* Check that the caller and the callee agree on passing S in memory:
   as a named argument, through va_arg (including the argument that
   follows it), and as a return value.  */

#include <stdarg.h>

struct B { int b : 5; };
struct __attribute__((packed)) S { char a; struct B s; };

__attribute__((noipa)) int
f (int n, ...)
{
  va_list ap;
  va_start (ap, n);
  struct S s = va_arg (ap, struct S);
  int i = va_arg (ap, int);
  va_end (ap);
  return s.a == 7 && s.s.b == -5 && i == 42;
}

__attribute__((noipa)) int
g (struct S s, int i)
{
  return s.a == 7 && s.s.b == -5 && i == 42;
}

__attribute__((noipa)) struct S
h (void)
{
  struct S s = { 7, { -5 } };
  return s;
}

int
main (void)
{
  struct S s = { 7, { -5 } };

  if (!f (0, s, 42))
    __builtin_abort ();
  if (!g (s, 42))
    __builtin_abort ();

  struct S r = h ();
  if (r.a != 7 || r.s.b != -5)
    __builtin_abort ();

  return 0;
}
