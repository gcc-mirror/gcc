/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float_strict } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 240

__attribute__ ((noinline))
int main1 ()
{
  int i;
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float diff;

  diff = 0;
  for (i = 0; i < N; i++) {
    diff += (b[i] - c[i]);
  }

  /* check results:  */
  if (diff != DIFF)
    abort ();

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 1 "vect" } } */
