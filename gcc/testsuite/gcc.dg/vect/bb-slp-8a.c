/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

unsigned int out[N];
unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (unsigned int x, unsigned int y, unsigned int *pin, unsigned int *pout)
{
  int i;
  unsigned int a0, a1, a2, a3;
 
  /* pin and pout may alias, and loads and stores are mixed.  The basic block
     cannot be vectorized.  */
  a0 = *pin++ + 23;
  *pout++ = a0 * x;
  a1 = *pin++ + 142;
  *pout++ = a1 * y;
  a2 = *pin++ + 2;
  *pout++ = a2 * x;
  a3 = *pin++ + 31;
  *pout++ = a3 * y;

  if (i)
    __asm__ volatile ("" : : : "memory");

  /* Check results.  */
  if (out[0] != (in[0] + 23) * x
      || out[1] != (in[1] + 142) * y
      || out[2] != (in[2] + 2) * x
      || out[3] != (in[3] + 31) * y)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (2, 3, &in[0], &out[0]);

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 0 "slp2" } } */
  
