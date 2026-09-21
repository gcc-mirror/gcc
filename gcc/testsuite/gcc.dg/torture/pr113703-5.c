/* { dg-do run { target lp64 } } */
/* { dg-additional-options "-fno-tree-vectorize -fno-tree-loop-distribute-patterns" } */

#include <stdint.h>

uintptr_t sum = 0;

__attribute__((noipa)) void
f (char *p, unsigned int i, unsigned int n)
{
  p -= i;
  do
    {
      sum += (uintptr_t)p;
      p -= 1;
      i++;
    }
  while (i < n);
}

int
main ()
{
  /* The number of iterations of the loop is (i + 1 > n) ? 0 : n - i - 1.
     I is -1, so i + 1 wraps around to 0 and is not greater than N, and the
     loop iterates twice.  IVOPTs used to replace the exit test by one on P
     with the bound P_0 + ((sizetype) i - (sizetype) n), i.e. P_0 + 0xfffffffe
     instead of P_0 - 2, and the loop ran away.  */
  f ((char *)0x10000ffffffff, -1, 1);
  /* SUM is 0x10000000000 + 0xfffffffffff.  */
  if (sum != (uintptr_t)0x1ffffffffffff)
    __builtin_abort ();
  return 0;
}
