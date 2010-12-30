/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } "*" "" } */

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

void
foo (void) throw (B,A)
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
