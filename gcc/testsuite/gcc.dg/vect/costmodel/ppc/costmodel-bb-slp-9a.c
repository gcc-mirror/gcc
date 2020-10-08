/* { dg-require-effective-target vect_int } */

#include "../../tree-vect.h"

#define N 16 

unsigned int out[N];
unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (void)
{
  unsigned int *pin = &in[1];
  unsigned int *pout = &out[0];

  /* Misaligned load.  */
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  /* Check results.  */
  if (out[0] != in[1]
      || out[1] != in[2]
      || out[2] != in[3]
      || out[3] != in[4])
    abort();

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp2"  { xfail  { vect_no_align && { ! vect_hw_misalign } } } } } */
