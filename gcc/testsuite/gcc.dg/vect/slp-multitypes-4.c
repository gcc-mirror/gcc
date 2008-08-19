/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 8 

short in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  int out[N*8];

  for (i = 0; i < N; i++)
    {
      out[i*8] = (int) in[i*8] + 1;
      out[i*8 + 1] = (int) in[i*8 + 1] + 2;
      out[i*8 + 2] = (int) in[i*8 + 2] + 3;
      out[i*8 + 3] = (int) in[i*8 + 3] + 4;
      out[i*8 + 4] = (int) in[i*8 + 4] + 5;
      out[i*8 + 5] = (int) in[i*8 + 5] + 6;
      out[i*8 + 6] = (int) in[i*8 + 6] + 7;
      out[i*8 + 7] = (int) in[i*8 + 7] + 8;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (out[i*8] !=  (int) in[i*8] + 1
         || out[i*8 + 1] != (int) in[i*8 + 1] + 2
         || out[i*8 + 2] != (int) in[i*8 + 2] + 3
         || out[i*8 + 3] != (int) in[i*8 + 3] + 4
         || out[i*8 + 4] != (int) in[i*8 + 4] + 5
         || out[i*8 + 5] != (int) in[i*8 + 5] + 6
         || out[i*8 + 6] != (int) in[i*8 + 6] + 7
         || out[i*8 + 7] != (int) in[i*8 + 7] + 8)
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  { target vect_unpack } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
