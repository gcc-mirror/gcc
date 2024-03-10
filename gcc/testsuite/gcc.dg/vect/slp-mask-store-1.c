/* { dg-additional-options "-mavx2" { target avx2 } } */

#include "tree-vect.h"

void __attribute__((noipa))
foo (unsigned * __restrict x, int * __restrict flag)
{
  for (int i = 0; i < 32; ++i)
    {
      if (flag[2*i+0])
        x[2*i+0] = x[2*i+0] + 3;
      if (flag[2*i+1])
        x[2*i+1] = x[2*i+1] + 177;
    }
}

unsigned x[16];
int flag[32] = { 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
unsigned res[16] = { 3, 177, 0, 0, 0, 177, 3, 0, 3, 177, 0, 0, 0, 177, 3, 0 };

int
main ()
{
  check_vect ();

  foo (x, flag);

  if (__builtin_memcmp (x, res, sizeof (x)) != 0)
    abort ();
  for (int i = 0; i < 32; ++i)
    if (flag[i] != 0 && flag[i] != 1)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" { target { vect_masked_store && vect_masked_load } } } } */
