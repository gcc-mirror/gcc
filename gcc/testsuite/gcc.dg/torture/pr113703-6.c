/* { dg-do run { target lp64 } } */
/* { dg-additional-options "-fno-tree-vectorize -fno-tree-loop-distribute-patterns" } */

#include <stdint.h>

uintptr_t sum = 0;

__attribute__((noipa)) void
f (char *p, unsigned int i, unsigned int n)
{
  p += i;
  do
    {
      sum += (uintptr_t)p;
      p += 1;
      i++;
    }
  while (i < n);
}

int
main ()
{
  /* Mirror image of gcc.dg/torture/pr113703-5.c with an increasing pointer:
     the loop iterates twice and IVOPTs used to compute the bound as
     P_0 - 0xfffffffe instead of P_0 + 2, making the loop exit at the first
     test.  */
  f ((char *)0xff00000001, -1, 1);
  /* SUM is 0x10000000000 + 0x10000000001.  */
  if (sum != (uintptr_t)0x20000000001)
    __builtin_abort ();
  return 0;
}
