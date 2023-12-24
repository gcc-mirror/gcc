/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

#ifndef N
#define N 30
#endif

#ifndef IDX
#define IDX 0
#endif

int n_earlyclobbers;

typedef void* rtx;
rtx reload_earlyclobbers[N] = {0};

rtx foo = (void*)0xbadf00d;

int
__attribute__((noinline, noipa))
earlyclobber_operand_p (rtx x)
{
  int i;

  for (i = 0; i < n_earlyclobbers; i++)
    if (reload_earlyclobbers[i] == x)
      return 1;

  return 0;
}

extern void abort ();

int main ()
{
  check_vect ();

  n_earlyclobbers = IDX;
  if (earlyclobber_operand_p (foo))
    abort ();

  return 0;
}
