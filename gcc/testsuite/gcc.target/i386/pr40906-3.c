/* { dg-do run { target *-*-linux* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fomit-frame-pointer -msse2 -mpush-args -mno-accumulate-outgoing-args" } */

#include "sse2-check.h"

void __attribute__((noinline))
f (__float128 a)
{
  if (a != 1.23Q)
    abort ();
}

int __attribute__((noinline))
g (__float128 b)
{
  f (b);
  return 0;
}

static void
sse2_test (void)
{
  g (1.23Q);
}
