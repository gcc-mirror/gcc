/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8 
unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
unsigned char in2[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  unsigned int out[N*8];
  unsigned char out2[N*8];

  for (i = 0; i < N/2; i++)
    {
      out[i*8] = in[i*8] + 5;
      out[i*8 + 1] = in[i*8 + 1] + 6;
      out[i*8 + 2] = in[i*8 + 2] + 7;
      out[i*8 + 3] = in[i*8 + 3] + 8;
      out[i*8 + 4] = in[i*8 + 4] + 9;
      out[i*8 + 5] = in[i*8 + 5] + 10;
      out[i*8 + 6] = in[i*8 + 6] + 11;
      out[i*8 + 7] = in[i*8 + 7] + 12;

      out2[i*16] = in2[i*16] + 2;
      out2[i*16 + 1] = in2[i*16 + 1] + 3;
      out2[i*16 + 2] = in2[i*16 + 2] + 4;
      out2[i*16 + 3] = in2[i*16 + 3] + 3;
      out2[i*16 + 4] = in2[i*16 + 4] + 2;
      out2[i*16 + 5] = in2[i*16 + 5] + 3;
      out2[i*16 + 6] = in2[i*16 + 6] + 2;
      out2[i*16 + 7] = in2[i*16 + 7] + 4;
      out2[i*16 + 8] = in2[i*16 + 8] + 2;
      out2[i*16 + 9] = in2[i*16 + 9] + 5;
      out2[i*16 + 10] = in2[i*16 + 10] + 2;
      out2[i*16 + 11] = in2[i*16 + 11] + 3;
      out2[i*16 + 12] = in2[i*16 + 12] + 4;
      out2[i*16 + 13] = in2[i*16 + 13] + 4;
      out2[i*16 + 14] = in2[i*16 + 14] + 3;
      out2[i*16 + 15] = in2[i*16 + 15] + 2;

    }

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (out[i*8] !=  in[i*8] + 5
         || out[i*8 + 1] != in[i*8 + 1] + 6
         || out[i*8 + 2] != in[i*8 + 2] + 7
         || out[i*8 + 3] != in[i*8 + 3] + 8
         || out[i*8 + 4] != in[i*8 + 4] + 9
         || out[i*8 + 5] != in[i*8 + 5] + 10
         || out[i*8 + 6] != in[i*8 + 6] + 11
         || out[i*8 + 7] != in[i*8 + 7] + 12
         || out2[i*16] !=  in2[i*16] + 2
         || out2[i*16 + 1] != in2[i*16 + 1] + 3
         || out2[i*16 + 2] != in2[i*16 + 2] + 4
         || out2[i*16 + 3] != in2[i*16 + 3] + 3
         || out2[i*16 + 4] != in2[i*16 + 4] + 2
         || out2[i*16 + 5] != in2[i*16 + 5] + 3
         || out2[i*16 + 6] != in2[i*16 + 6] + 2
         || out2[i*16 + 7] != in2[i*16 + 7] + 4
         || out2[i*16 + 8] != in2[i*16 + 8] + 2
         || out2[i*16 + 9] != in2[i*16 + 9] + 5
         || out2[i*16 + 10] != in2[i*16 + 10] + 2
         || out2[i*16 + 11] != in2[i*16 + 11] + 3
         || out2[i*16 + 12] != in2[i*16 + 12] + 4
         || out2[i*16 + 13] != in2[i*16 + 13] + 4
         || out2[i*16 + 14] != in2[i*16 + 14] + 3
         || out2[i*16 + 15] != in2[i*16 + 15] + 2)
 
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail sparc*-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { xfail sparc*-*-* } } } */
