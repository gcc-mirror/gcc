/* Check vectorization of uncounted reductions.  */
/* { dg-add-options vect_early_break } */
/* { dg-do run } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
#include <assert.h>
#include "tree-vect.h"

__attribute__((noipa, noinline))
int
foo (int *a0, int *aN, int accum)
{
  int i = 0;
  while (1)
    {
      if (a0[i++] == *aN)
	return accum;
      accum += a0[i];
     }
}

void
accum_check (int *a0, int len, int accum, int result)
{
  int retval = foo (a0, a0+len, accum);
  assert (retval == result);
}

int
main (void)
{
  int a[] = {0,1,2,3,4,5,6,10,11,12};
  accum_check (a, 5, 0, 15);
  accum_check (a, 9, 0, 54);
  accum_check (a, 6, 0, 21);
  accum_check (a, 6, 5, 26);
  return 0;
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "Detected reduction." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
