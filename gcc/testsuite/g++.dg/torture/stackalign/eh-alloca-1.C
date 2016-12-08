/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } "*" "" } */

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
foo (int size)
#if __cplusplus <= 201402L
throw (B,A)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  char *p = (char*) __builtin_alloca (size + 1);
  aligned i;

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

int
main()
{
  try {	foo (5); }
  catch (A& a) { }
  return 0;
}
