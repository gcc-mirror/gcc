/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O3 -fno-tree-forwprop -fno-tree-vrp" } */

#include "tree-vect.h"

__attribute__((noipa))
int n(long o)
{
  long x = 536870911 - 1 / o;
  long g = (x ^ 3124462952626119202L) & 5121363712956983201L;
  long j = ~g;
  long d = x ^ -3124462952626119203L;
  long i = d & ~5121363712956983201L;
  long c = ~i;
  long e = (c & j) ^ 7804438717590242691L;
  long f = 2147483647 / e;
  return f & 5;
}

int main()
{
  check_vect ();
  volatile long o = 809516896;
  if (n(o) != 4)
    __builtin_abort();
  return 0;
}
