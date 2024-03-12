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
#pragma GCC novector
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
#pragma GCC novector
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
#pragma GCC novector
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
/* Some targets can vectorize the second of the three main loops using
   hybrid SLP.  For 128-bit vectors, the required 4->3 permutations are:

   { 0, 1, 2, 4, 5, 6, 8, 9 }
   { 2, 4, 5, 6, 8, 9, 10, 12 }
   { 5, 6, 8, 9, 10, 12, 13, 14 }

   Not all vect_perm targets support that, and it's a bit too specific to have
   its own effective-target selector, so we just test targets directly.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "vect" { target { powerpc64*-*-* s390*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { vect_strided4 && { ! { powerpc64*-*-* s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect"  { target { ! { vect_strided4 } } } } } */
  
