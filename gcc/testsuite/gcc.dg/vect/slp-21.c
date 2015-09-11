/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int
main1 ()
{
  unsigned short i;
  unsigned short out[N*8], out2[N*8], b0, b1, b2, b3, b4, a0, a1, a2, a3, b5;
  unsigned short in[N*8];

  for (i = 0; i < N*8; i++)
    {
      in[i] = i;
    }

  /* Different operations in both cases - vectorization with interleaving.  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 * 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 * 5;
      
      b4 = a2 + 4;
      b5 = a3 + 3;

      out[i*4] = b0;
      out[i*4 + 1] = b1;
      out[i*4 + 2] = b2;
      out[i*4 + 3] = b3;

      out2[i*4] = b0;
      out2[i*4 + 1] = b1;
      out2[i*4 + 2] = b4;
      out2[i*4 + 3] = b5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 * 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 * 5;

      b4 = a2 + 4;
      b5 = a3 + 3;

      if (out[i*4] != b0 
         || out[i*4 + 1] != b1
         || out[i*4 + 2] != b2
         || out[i*4 + 3] != b3)
	abort ();

      if (out2[i*4] != b0  
         || out2[i*4 + 1] != b1
         || out2[i*4 + 2] != b4
         || out2[i*4 + 3] != b5)
        abort ();
    }

  /* Different operations in the first case - vectorization with interleaving.  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 * 5;

      b4 = a2 + 4;
      b5 = a3 + 3;

      out[i*4] = b0;
      out[i*4 + 1] = b1;
      out[i*4 + 2] = b2;
      out[i*4 + 3] = b3;

      out2[i*4] = b0;
      out2[i*4 + 1] = b1;
      out2[i*4 + 2] = b4;
      out2[i*4 + 3] = b5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 * 5;

      b4 = a2 + 4;
      b5 = a3 + 3;

      if (out[i*4] != b0
         || out[i*4 + 1] != b1
         || out[i*4 + 2] != b2
         || out[i*4 + 3] != b3)
        abort ();

      if (out2[i*4] != b0
         || out2[i*4 + 1] != b1
         || out2[i*4 + 2] != b4
         || out2[i*4 + 3] != b5)
        abort ();
    }


  /* Different operations in the second case - vectorization with interleaving.  */
  for (i = 0; i < N; i++) 
    { 
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 + 5;

      b4 = a2 * 4;
      b5 = a3 + 3;

      out[i*4] = b0;
      out[i*4 + 1] = b1;
      out[i*4 + 2] = b2;
      out[i*4 + 3] = b3;

      out2[i*4] = b0;
      out2[i*4 + 1] = b1;
      out2[i*4 + 2] = b4;
      out2[i*4 + 3] = b5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 + 5;

      b4 = a2 * 4;
      b5 = a3 + 3;

      if (out[i*4] != b0
         || out[i*4 + 1] != b1
         || out[i*4 + 2] != b2
         || out[i*4 + 3] != b3)
        abort ();

      if (out2[i*4] != b0
         || out2[i*4 + 1] != b1
         || out2[i*4 + 2] != b4
         || out2[i*4 + 3] != b5)
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

/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect"  { target { vect_strided4 || vect_extract_even_odd } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target  { ! { vect_strided4 || vect_extract_even_odd } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_strided4 }  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect"  { target { ! { vect_strided4 } } } } } */
  
