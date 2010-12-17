/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

unsigned short out[N];
unsigned short in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

int i;
__attribute__ ((noinline)) int
main1 ()
{
  unsigned short *pin = &in[0];
  unsigned short *pout = &out[0];
  
  *pout++ = *pin++;
  *pout++ = *pin++;

  if (i)
    __asm__ volatile ("" : : : "memory");

  /* Check results.  */
  if (out[0] != in[0]
      || out[1] != in[1])
    abort();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized using SLP" 0 "slp" } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
  
