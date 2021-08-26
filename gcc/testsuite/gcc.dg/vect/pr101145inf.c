/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O3" } */
#include <limits.h>
#include "pr101145inf.inc"

__attribute__ ((noinline))
unsigned foo(unsigned val, unsigned start)
{
  unsigned cnt = 0;
  for (unsigned i = start; val <= i; i+=16)
    cnt++;
  return cnt;
}

void test_finite ()
{
  unsigned n = foo (16, UINT_MAX - 32);
  if (n != 3)
    __builtin_abort ();
}

void test_infinite ()
{
 foo (15, UINT_MAX - 32);
}
