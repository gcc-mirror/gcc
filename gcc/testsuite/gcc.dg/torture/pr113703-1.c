/* { dg-do run } */
/* { dg-additional-options "-fno-tree-vectorize -fno-tree-loop-distribute-patterns" } */

#include <stdint.h>

char a[16];

__attribute__((noipa)) void
f (char *p, uintptr_t i, uintptr_t n)
{
  p += i;
  do
    {
      *p = 1;
      p += 1;
      i++;
    }
  while (i < n);
}

int
main ()
{
  /* I is -2 and thus not less than N, so the loop stores to A[0] only.
     IVOPTs used to replace the exit test by P < (A + 2) + N, which does
     not hold at the first test and makes the loop iterate on.  */
  f (a + 2, -2, 8);
  if (a[0] != 1)
    __builtin_abort ();
  for (int j = 1; j < 16; j++)
    if (a[j] != 0)
      __builtin_abort ();
  return 0;
}
