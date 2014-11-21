/* { dg-do run } */
/* { dg-options "-O2 -ftree-pre -fno-tree-loop-im -fgcse-sm -fdump-rtl-store_motion" } */

/* tree-pre moves the *sum load out of the loop.  ftree-loop-im moves the *sum
   store out of the loop, so we disable it, to allow fgcse-sm to do it
   instead.  */

#include <stdlib.h>

void __attribute__((noinline))
f (unsigned int *__restrict__ a, unsigned int *__restrict__ sum, unsigned int n)
{
  unsigned int i;
  for (i = 0; i < n; ++i)
    *sum += a[i];
}

int
main ()
{
  unsigned int a[] = { 1, 10, 100 };
  unsigned sum = 1000;

  f (a, &sum, 3);
  if (sum != 1111)
    abort ();

  return 0;
}

/* Check that -fgcse-sm did something for f.  */
/* { dg-final { scan-rtl-dump "STORE_MOTION of f, .* basic blocks, 1 insns deleted, 1 insns created" "store_motion" } } */
