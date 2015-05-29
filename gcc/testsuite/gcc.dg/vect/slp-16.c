/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8 

unsigned int out[N*8];
unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
unsigned int in2[N*16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
unsigned int out2[N*16];

int
main1 ()
{
  int i;
  unsigned int a0, a1, a2, a3, a4, a5, a6, a7, b1, b0, b2, b3, b4, b5, b6, b7;

  /* SLP group of size that is not a multiple of vector size. 
     Unrolling by 2.  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*2] + 5;
      a1 = in[i*2 + 1] + 6; 
    
      b0 = a0 * 3;
      b1 = a1 * 2; 
    
      out[i*2] = b0 - 2;
      out[i*2 + 1] = b1 - 3; 
    
      out2[i*6] = in2[i*6] * 2;
      out2[i*6 + 1] = in2[i*6 + 1] * 3;
      out2[i*6 + 2] = in2[i*6 + 2] * 4;
      out2[i*6 + 3] = in2[i*6 + 3] * 2;
      out2[i*6 + 4] = in2[i*6 + 4] * 4;
      out2[i*6 + 5] = in2[i*6 + 5] * 3;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (out[i*2] !=  (in[i*2] + 5) * 3 - 2
         || out[i*2 + 1] != (in[i*2 + 1] + 6) * 2 - 3
         || out2[i*6] != in2[i*6] * 2
         || out2[i*6 + 1] != in2[i*6 + 1] * 3
         || out2[i*6 + 2] != in2[i*6 + 2] * 4
         || out2[i*6 + 3] != in2[i*6 + 3] * 2
         || out2[i*6 + 4] != in2[i*6 + 4] * 4
         || out2[i*6 + 5] != in2[i*6 + 5] * 3)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_int_mult } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_int_mult } } } */
  
