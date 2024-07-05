/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int
main1 (unsigned short a0, unsigned short a1, unsigned short a2, 
       unsigned short a3, unsigned short a4, unsigned short a5,
       unsigned short a6, unsigned short a7, unsigned short a8,
       unsigned short a9, unsigned short a10, unsigned short a11,
       unsigned short a12, unsigned short a13, unsigned short a14,
       unsigned short a15)
{
  int i;
  unsigned short out[N*16];

  for (i = 0; i < N; i++)
    {
      out[i*4] = a8;
      out[i*4 + 1] = a1;
      out[i*4 + 2] = a2;
      out[i*4 + 3] = a3;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*4] != a8 
         || out[i*4 + 1] != a1
         || out[i*4 + 2] != a2
         || out[i*4 + 3] != a3)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      out[i*16] = a8;
      out[i*16 + 1] = a7;
      out[i*16 + 2] = a1;
      out[i*16 + 3] = a2;
      out[i*16 + 4] = a8;
      out[i*16 + 5] = a5;
      out[i*16 + 6] = a5;
      out[i*16 + 7] = a4;
      out[i*16 + 8] = a12;
      out[i*16 + 9] = a13;
      out[i*16 + 10] = a14;
      out[i*16 + 11] = a15;
      out[i*16 + 12] = a6;
      out[i*16 + 13] = a9;
      out[i*16 + 14] = a0;
      out[i*16 + 15] = a7;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*16] != a8
          || out[i*16 + 1] != a7
          || out[i*16 + 2] != a1
          || out[i*16 + 3] != a2
          || out[i*16 + 4] != a8
          || out[i*16 + 5] != a5
          || out[i*16 + 6] != a5
          || out[i*16 + 7] != a4
          || out[i*16 + 8] != a12
          || out[i*16 + 9] != a13
          || out[i*16 + 10] != a14
          || out[i*16 + 11] != a15
          || out[i*16 + 12] != a6
          || out[i*16 + 13] != a9
          || out[i*16 + 14] != a0
          || out[i*16 + 15] != a7)
        abort ();
    }

  /* SLP with unrolling by 8.  */
  for (i = 0; i < N; i++)
    {
      out[i*3] = a8;
      out[i*3 + 1] = a1;
      out[i*3 + 2] = a2;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*3] != a8
         || out[i*3 + 1] != a1
         || out[i*3 + 2] != a2)
        abort ();
    }

  /* SLP with unrolling by 8.  */
  for (i = 0; i < N; i++)
    {
      out[i*11] = a8;
      out[i*11 + 1] = a7;
      out[i*11 + 2] = a1;
      out[i*11 + 3] = a2;
      out[i*11 + 4] = a8;
      out[i*11 + 5] = a5;
      out[i*11 + 6] = a5;
      out[i*11 + 7] = a4;
      out[i*11 + 8] = a12;
      out[i*11 + 9] = a13;
      out[i*11 + 10] = a14;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*11] != a8
          || out[i*11 + 1] != a7
          || out[i*11 + 2] != a1
          || out[i*11 + 3] != a2
          || out[i*11 + 4] != a8
          || out[i*11 + 5] != a5
          || out[i*11 + 6] != a5
          || out[i*11 + 7] != a4
          || out[i*11 + 8] != a12
          || out[i*11 + 9] != a13
          || out[i*11 + 10] != a14)
        abort ();
    }


  return 0;
}

int main (void)
{
  check_vect ();

  main1 (15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "vect" } } */
  
