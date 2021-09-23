/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 12

unsigned short in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};

int
main1 ()
{
  int i;
  unsigned short out[N*8];

  for (i = 0; i < N; i++)
    {
      out[i*8] = in[i*8];
      out[i*8 + 1] = in[i*8 + 1];
      out[i*8 + 2] = in[i*8 + 2];
      out[i*8 + 3] = in[i*8 + 3];
      out[i*8 + 4] = in[i*8 + 4];
      out[i*8 + 5] = in[i*8 + 5];
      out[i*8 + 6] = in[i*8 + 6];
      out[i*8 + 7] = in[i*8 + 7];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (out[i*8] !=  in[i*8]
         || out[i*8 + 1] != in[i*8 + 1]
         || out[i*8 + 2] != in[i*8 + 2]
         || out[i*8 + 3] != in[i*8 + 3]
         || out[i*8 + 4] != in[i*8 + 4]
         || out[i*8 + 5] != in[i*8 + 5]
         || out[i*8 + 6] != in[i*8 + 6]
         || out[i*8 + 7] != in[i*8 + 7])
	abort ();
    }

  for (i = 0; i < N*2; i++)
    {
      out[i*4] = in[i*4];
      out[i*4 + 1] = in[i*4 + 1];
      out[i*4 + 2] = in[i*4 + 2];
      out[i*4 + 3] = in[i*4 + 3];
    }

  /* check results:  */
  for (i = 0; i < N*2; i++)
    {
      if (out[i*4] !=  in[i*4]
         || out[i*4 + 1] != in[i*4 + 1]
         || out[i*4 + 2] != in[i*4 + 2]
         || out[i*4 + 3] != in[i*4 + 3])
        abort ();
    }

  for (i = 0; i < N/2; i++)
    {
      out[i*16] = in[i*16];
      out[i*16 + 1] = in[i*16 + 1];
      out[i*16 + 2] = in[i*16 + 2];
      out[i*16 + 3] = in[i*16 + 3];
      out[i*16 + 4] = in[i*16 + 4];
      out[i*16 + 5] = in[i*16 + 5];
      out[i*16 + 6] = in[i*16 + 6];
      out[i*16 + 7] = in[i*16 + 7];
      out[i*16 + 8] = in[i*16 + 8];
      out[i*16 + 9] = in[i*16 + 9];
      out[i*16 + 10] = in[i*16 + 10];
      out[i*16 + 11] = in[i*16 + 11];
      out[i*16 + 12] = in[i*16 + 12];
      out[i*16 + 13] = in[i*16 + 13];
      out[i*16 + 14] = in[i*16 + 14];
      out[i*16 + 15] = in[i*16 + 15];
    }

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (out[i*16] !=  in[i*16]
         || out[i*16 + 1] != in[i*16 + 1]
         || out[i*16 + 2] != in[i*16 + 2]
         || out[i*16 + 3] != in[i*16 + 3]
         || out[i*16 + 4] != in[i*16 + 4]
         || out[i*16 + 5] != in[i*16 + 5]
         || out[i*16 + 6] != in[i*16 + 6]
         || out[i*16 + 7] != in[i*16 + 7]
         || out[i*16 + 8] != in[i*16 + 8]
         || out[i*16 + 9] != in[i*16 + 9]
         || out[i*16 + 10] != in[i*16 + 10]
         || out[i*16 + 11] != in[i*16 + 11]
         || out[i*16 + 12] != in[i*16 + 12]
         || out[i*16 + 13] != in[i*16 + 13]
         || out[i*16 + 14] != in[i*16 + 14]
         || out[i*16 + 15] != in[i*16 + 15])
        abort ();
    }

  /* SLP with unrolling by 8.  */
  for (i = 0; i < N/4; i++)
    {
      out[i*9] = in[i*9];
      out[i*9 + 1] = in[i*9 + 1];
      out[i*9 + 2] = in[i*9 + 2];
      out[i*9 + 3] = in[i*9 + 3];
      out[i*9 + 4] = in[i*9 + 4];
      out[i*9 + 5] = in[i*9 + 5];
      out[i*9 + 6] = in[i*9 + 6];
      out[i*9 + 7] = in[i*9 + 7];
      out[i*9 + 8] = in[i*9 + 8];
    }

  /* check results:  */
  for (i = 0; i < N/4; i++)
    {
      if (out[i*9] !=  in[i*9]
         || out[i*9 + 1] != in[i*9 + 1]
         || out[i*9 + 2] != in[i*9 + 2]
         || out[i*9 + 3] != in[i*9 + 3]
         || out[i*9 + 4] != in[i*9 + 4]
         || out[i*9 + 5] != in[i*9 + 5]
         || out[i*9 + 6] != in[i*9 + 6]
         || out[i*9 + 7] != in[i*9 + 7]
         || out[i*9 + 8] != in[i*9 + 8])
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

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { target { ! { vect_partial_vectors || vect32 } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect" { target { vect_partial_vectors || vect32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect" { target { ! { vect_partial_vectors || vect32 } } } } }*/
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "vect" { target { vect_partial_vectors || vect32 } } } } */
  
