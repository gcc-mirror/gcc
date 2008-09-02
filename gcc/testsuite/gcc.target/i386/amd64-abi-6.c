/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

#include <stdarg.h>
#include <assert.h>

int n1 = 30;
double n2 = 324;
double n3 = 39494.94;
double n4 = 407;
double n5 = 32.304;
double n6 = 394.14;
double n7 = 4.07;
double n8 = 32.4;
double n9 = 314.194;
double n10 = 0.1407;

int e1;
double e2;
double e3;
double e4;
double e5;
double e6;
double e7;
double e8;
double e9;
double e10;

static void
__attribute__((noinline))
foo (va_list va_arglist)
{
  e2 = va_arg (va_arglist, double);
  e3 = va_arg (va_arglist, double);
  e4 = va_arg (va_arglist, double);
  e5 = va_arg (va_arglist, double);
  e6 = va_arg (va_arglist, double);
  e7 = va_arg (va_arglist, double);
  e8 = va_arg (va_arglist, double);
  e9 = va_arg (va_arglist, double);
  e10 = va_arg (va_arglist, double);
}

static void
__attribute__((noinline))
test (int a1, ...)
{
  va_list va_arglist;
  e1 = a1;
  va_start (va_arglist, a1);
  foo (va_arglist);
  va_end (va_arglist);
}

int
main ()
{
  test (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10);
  assert (n1 == e1);
  assert (n2 == e2);
  assert (n3 == e3);
  assert (n4 == e4);
  assert (n5 == e5);
  assert (n6 == e6);
  assert (n7 == e7);
  assert (n8 == e8);
  assert (n9 == e9);
  assert (n10 == e10);
  return 0;
}
