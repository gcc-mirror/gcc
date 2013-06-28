/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

int b[N];
unsigned int out[N];
unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (unsigned int x, unsigned int y)
{
  int i;
  unsigned int a0, a1, a2, a3;

  if (x > y)
    x = x + y;
  else
    y = x;

  /* Two SLP instances in the basic block.  */
  a0 = in[0] + 23;
  a1 = in[1] + 142;
  a2 = in[2] + 2;
  a3 = in[3] + 31;
 
  b[0] = -a0;
  b[1] = -a1;
  b[2] = -a2;
  b[3] = -a3;
  
  out[0] = a0 * x;
  out[1] = a1 * y;
  out[2] = a2 * x;
  out[3] = a3 * y;

  if (x)
    __asm__ volatile ("" : : : "memory");

  /* Check results.  */
  if (out[0] != (in[0] + 23) * x
      || out[1] != (in[1] + 142) * y
      || out[2] != (in[2] + 2) * x
      || out[3] != (in[3] + 31) * y
      || b[0] != -(in[0] + 23)
      || b[1] != -(in[1] + 142)
      || b[2] != -(in[2] + 2)
      || b[3] != -(in[3] + 31))
      
    abort();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (2, 3);

  return 0;
}

/* { dg-final { scan-tree-dump-times "Vectorized basic-block" 1 "slp" { target vect_int_mult } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "slp" { target vect_int_mult } } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
  
