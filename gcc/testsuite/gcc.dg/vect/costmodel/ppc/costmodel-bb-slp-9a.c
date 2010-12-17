/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 16 

unsigned int out[N];
unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (unsigned int x, unsigned int y)
{
  int i;
  unsigned int *pin = &in[1];
  unsigned int *pout = &out[0];
  unsigned int a0, a1, a2, a3;

  /* Misaligned load.  */
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;

  /* Check results.  */
  if (out[0] != in[1]
      || out[1] != in[2]
      || out[2] != in[3]
      || out[3] != in[4])
    abort();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (2, 3);

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized using SLP" 1 "slp"  { xfail  vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
  
