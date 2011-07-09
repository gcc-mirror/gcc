/* { dg-do run { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mno-sse" } */

#include <stdarg.h>
#include <assert.h>

int n1 = 30;
int n2 = 324;
void *n3 = (void *) &n2;
int n4 = 407;

int e1;
int e2;
void *e3;
int e4;

static void
__attribute__((noinline))
foo (va_list va_arglist)
{
  e2 = va_arg (va_arglist, int);
  e3 = va_arg (va_arglist, void *);
  e4 = va_arg (va_arglist, int);
}

static void
__attribute__((noinline))
test (int a1, ...)
{
  e1 = a1;
  va_list va_arglist;
  va_start (va_arglist, a1);
  foo (va_arglist);
  va_end (va_arglist);
}

int
main ()
{
  test (n1, n2, n3, n4);
  assert (n1 == e1);
  assert (n2 == e2);
  assert (n3 == e3);
  assert (n4 == e4);
  return 0;
}
