/* { dg-options "-Wno-abi" {target arm_eabi} } */
/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } "*" "" } */

#include <stdarg.h>
#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int global;

void
bar (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

class Base {};

struct A : virtual public Base
{
  A() {}
};

struct B {};

void
test (va_list arg)
#if __cplusplus <= 201402L
throw (B,A)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  char *p;
  aligned i;
  int size;
  double x;

  size = va_arg (arg, int);
  if (size != 5)
    abort ();

  p = (char *) __builtin_alloca (size + 1);

  x = va_arg (arg, double);
  if (x != 5.0)
    abort ();

  bar (p, size);
  if (__builtin_strncmp (p, "good", size) != 0)
    {
#ifdef DEBUG
      p[size] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
      abort ();
    }

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();

  throw A();
}

void
foo (const char *fmt, ...)
{
  va_list arg;
  va_start (arg, fmt);
  test (arg);
  va_end (arg);
}
int
main()
{
  try {	foo ("foo", 5, 5.0); }
  catch (A& a) { }
  return 0;
}
