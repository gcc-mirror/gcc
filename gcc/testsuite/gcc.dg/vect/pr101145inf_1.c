/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O3" } */
#include <limits.h>
#include "pr101145inf.inc"

__attribute__ ((noinline))
unsigned foo(unsigned val, unsigned start)
{
  unsigned cnt = 0;
  for (unsigned i = start; i < val; i-=16)
    cnt++;
  return cnt;
}

void test_finite ()
{
  foo (UINT_MAX - 15, 32);
}

void test_infinite ()
{
  foo (UINT_MAX - 14, 32);
}
