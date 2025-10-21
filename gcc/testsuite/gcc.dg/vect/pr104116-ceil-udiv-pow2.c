/* { dg-additional-options "-fgimple -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_int} */
/* { dg-require-effective-target vect_condition} */
/* { dg-require-effective-target vect_shift} */



#include "pr104116.h"
#include "tree-vect.h"

TEST_FN_UNSIGNED(__CEIL_DIV, 8u, udiv)

int main (void)
{
  check_vect ();
  unsigned int *ua = (unsigned int*)&uarr;
  init_uarr(ua, N);
  udiv(ua);
  #pragma GCC novector
  for (int i=0; i<N; i++)
  {
    unsigned int expected = cl_udiv (0xf0000000 + i, 8);
    if (expected != ua[i])
      abort();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 1 "vect" } } */