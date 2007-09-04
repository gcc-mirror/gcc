/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

static __attribute__ ((noinline)) int
main1 (void)
{
  int i;
  float a[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  /* Too conservative dependence test.  */
  for (i = 0; i < N - 1; i++){
    a[i] = b[i] + c[i];
    a[i+1] = b[i] + c[i];
  }

  /* Check results.  */
  for (i = 0; i < N - 1; i++){
    if (a[i] != b[i] + c[i])
	abort ();
  }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}
     
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


