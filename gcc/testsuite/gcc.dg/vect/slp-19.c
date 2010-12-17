/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

int
main1 ()
{
  unsigned int i;
  unsigned int out[N*8];
  unsigned int in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
  unsigned int ia[N*2], a0, a1, a2, a3;

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
    
      ia[i] = in[i*8 + 2];
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
         || out[i*8 + 7] != in[i*8 + 7]
         || ia[i] != in[i*8 + 2])
	abort ();
    }

  for (i = 0; i < N*2; i++)
    {
      a0 = in[i*4] + 1;
      a1 = in[i*4 + 1] + 2;
      a2 = in[i*4 + 2] + 3;
      a3 = in[i*4 + 3] + 4;

      out[i*4] = a0;
      out[i*4 + 1] = a1;
      out[i*4 + 2] = a2;
      out[i*4 + 3] = a3;

      ia[i] = a2;
    }

  /* check results:  */
  for (i = 0; i < N*2; i++)
    {
      if (out[i*4] !=  in[i*4] + 1
         || out[i*4 + 1] != in[i*4 + 1] + 2
         || out[i*4 + 2] != in[i*4 + 2] + 3
         || out[i*4 + 3] != in[i*4 + 3] + 4
         || ia[i] != in[i*4 + 2] + 3)
        abort ();
    }

  /* The last stmt requires interleaving of not power of 2 size - not 
     vectorizable.  */
  for (i = 0; i < N/2; i++)
    {
      out[i*12] = in[i*12];
      out[i*12 + 1] = in[i*12 + 1];
      out[i*12 + 2] = in[i*12 + 2];
      out[i*12 + 3] = in[i*12 + 3];
      out[i*12 + 4] = in[i*12 + 4];
      out[i*12 + 5] = in[i*12 + 5];
      out[i*12 + 6] = in[i*12 + 6];
      out[i*12 + 7] = in[i*12 + 7];
      out[i*12 + 8] = in[i*12 + 8];
      out[i*12 + 9] = in[i*12 + 9];
      out[i*12 + 10] = in[i*12 + 10];
      out[i*12 + 11] = in[i*12 + 11];

      ia[i] = in[i*12 + 7];
    }

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (out[i*12] !=  in[i*12]
         || out[i*12 + 1] != in[i*12 + 1]
         || out[i*12 + 2] != in[i*12 + 2]
         || out[i*12 + 3] != in[i*12 + 3]
         || out[i*12 + 4] != in[i*12 + 4]
         || out[i*12 + 5] != in[i*12 + 5]
         || out[i*12 + 6] != in[i*12 + 6]
         || out[i*12 + 7] != in[i*12 + 7]
         || out[i*12 + 8] != in[i*12 + 8]
         || out[i*12 + 9] != in[i*12 + 9]
         || out[i*12 + 10] != in[i*12 + 10]
         || out[i*12 + 11] != in[i*12 + 11]
         || ia[i] != in[i*12 + 7])
        abort ();
    }

  /* Hybrid SLP with unrolling by 2.  */
  for (i = 0; i < N; i++)
    {
      out[i*6] = in[i*6];
      out[i*6 + 1] = in[i*6 + 1];
      out[i*6 + 2] = in[i*6 + 2];
      out[i*6 + 3] = in[i*6 + 3];
      out[i*6 + 4] = in[i*6 + 4];
      out[i*6 + 5] = in[i*6 + 5];
    
      ia[i] = i;
    } 
    
  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (out[i*6] !=  in[i*6]
         || out[i*6 + 1] != in[i*6 + 1]
         || out[i*6 + 2] != in[i*6 + 2]
         || out[i*6 + 3] != in[i*6 + 3]
         || out[i*6 + 4] != in[i*6 + 4]
         || out[i*6 + 5] != in[i*6 + 5]
         || ia[i] != i)
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

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { target  vect_strided_wide  } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target  { ! { vect_strided_wide } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect"  { target  vect_strided_wide  } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  { target { ! { vect_strided_wide } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
