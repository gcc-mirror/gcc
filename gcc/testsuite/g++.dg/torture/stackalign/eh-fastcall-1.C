/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

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

__attribute__ ((fastcall))
void
foo (int j, int k, int m, int n, int o) throw (B,A)
{
  aligned i;

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();

  if (i != 20 || j != 1 || k != 2 || m != 3 || n != 4 || o != 5)
    abort ();

  throw A();
}

int
main()
{
  try { foo (1, 2, 3, 4, 5); }
  catch (A& a) { }
  return 0;
}
