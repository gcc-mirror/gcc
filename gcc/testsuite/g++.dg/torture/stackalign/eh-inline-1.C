/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int global;

class Base {};

struct A : virtual public Base
{
  A() {}
};

struct B {};

static void
inline __attribute__((always_inline))
foo (void)
#if __cplusplus <= 201402L
throw (B,A)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
  aligned i;

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();
  throw A();
}

int
main()
{
  try {	foo (); }
  catch (A& a) { }
  return 0;
}
