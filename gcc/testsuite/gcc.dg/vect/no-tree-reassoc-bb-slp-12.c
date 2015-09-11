/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

unsigned int out[N];
unsigned int in1[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
unsigned int in2[N] = {10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115};

__attribute__ ((noinline)) int
main1 (unsigned int x, unsigned int y)
{
  int i;
  unsigned int *pin1 = &in1[0];
  unsigned int *pin2 = &in2[0];
  unsigned int *pout = &out[0];
  unsigned int a0, a1, a2, a3;

  a0 = *pin2++ - *pin1++ + 23;
  a1 = *pin2++ - *pin1++ + 142;
  a2 = *pin2++ - *pin1++ + 2;
  a3 = *pin2++ - *pin1++ + 31;
  
  *pout++ = a0 * x;
  *pout++ = a1 * y;
  *pout++ = a2 * x;
  *pout++ = a3 * y;

  /* Check results.  */
  if (out[0] != (in2[0] - in1[0] + 23) * x
      || out[1] != (in2[1] - in1[1] + 142) * y
      || out[2] != (in2[2] - in1[2] + 2) * x
      || out[3] != (in2[3] - in1[3] + 31) * y)
    abort();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (2, 3);

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp2" { target vect_int_mult } } } */
  
