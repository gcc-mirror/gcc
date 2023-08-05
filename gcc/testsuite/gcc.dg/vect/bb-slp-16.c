/* { dg-require-effective-target vect_int } */
/* The SLP vectorization happens as part of the if-converted loop body.  */
/* { dg-additional-options "-fdump-tree-vect-details" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32 

unsigned int out[N*8];
unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
unsigned int arr[N] = {0,1,2,3,4,5,6,7};

__attribute__ ((noinline)) int
main1 (int dummy)
{
  int i;
  unsigned int *pin = &in[0];
  unsigned int *pout = &out[0];
  unsigned int a = 0;
  
  for (i = 0; i < N; i++)
    {
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      *pout++ = *pin++ + a;
      if (arr[i] = i)
        a = i;
      else
        a = 2;
    }

  a = 0;
  /* check results: */ 
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*8] !=  in[i*8] + a
         || out[i*8 + 1] != in[i*8 + 1] + a
         || out[i*8 + 2] != in[i*8 + 2] + a
         || out[i*8 + 3] != in[i*8 + 3] + a
         || out[i*8 + 4] != in[i*8 + 4] + a
         || out[i*8 + 5] != in[i*8 + 5] + a
         || out[i*8 + 6] != in[i*8 + 6] + a
         || out[i*8 + 7] != in[i*8 + 7] + a)
	abort ();

      if (arr[i] = i)
        a = i;
      else
        a = 2;
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (33);

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "vect" } } */
