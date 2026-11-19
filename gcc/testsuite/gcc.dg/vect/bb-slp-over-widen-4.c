/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O3 -fno-tree-forwprop -fno-tree-vrp" } */

#include "tree-vect.h"

struct S {
  unsigned long a : 4;
};

__attribute__((noinline))
int n(long o, struct S *sp)
{
  long x = 536870911 - 1 / o;
  long g = (x ^ 3124462952626119202L) & 5121363712956983201L;
  long j = ~g;
  long d = x ^ -3124462952626119203L;
  long i = d & ~5121363712956983201L;
  long c = ~i;
  long e = (c & j) ^ 7804438717590242691L;
  long s = e >> 28;
  long f = 1L << s;
  sp->a = f;
  return sp->a;
}

int main()
{
  check_vect ();
  volatile long o = 809516896;
  struct S sp;
  if (n(o, &sp) != 2)
    __builtin_abort();
  return 0;
}
