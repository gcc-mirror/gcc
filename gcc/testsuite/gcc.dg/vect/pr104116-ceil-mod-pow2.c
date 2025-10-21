/* { dg-additional-options "-fgimple -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_int} */
/* { dg-require-effective-target vect_condition} */
/* { dg-require-effective-target vect_shift} */



#include "pr104116.h"
#include "tree-vect.h"


TEST_FN(__CEIL_MOD, 8, div)

int main (void)
{
  check_vect ();
  unsigned int *a = (unsigned int*)&arr;
  init_arr(a, N);
  div(a);
  #pragma GCC novector
  for (int i=0; i<N; i++)
  {
    unsigned int expected = cl_mod (i - N/2, 8);
    if (expected != a[i])
      abort();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 1 "vect" } } */