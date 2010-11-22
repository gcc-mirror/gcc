/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int
main1 (unsigned short a0, unsigned short a1, unsigned short a2, 
       unsigned short a3, unsigned short a4, unsigned short a5,
       unsigned short a6, unsigned short a7, unsigned short a8)
{
  int i;
  unsigned short out[N*8], out2[N*8], out3[N*8], b0, b1, b2, b3, b4, b5, b6, b7, b8;

  for (i = 0; i < N; i++)
    {
      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 + 5;
      b4 = a4 + 4;
      b5 = a5 + 3;

      out[i*4] = b0;
      out[i*4 + 1] = b1;
      out[i*4 + 2] = b2;
      out[i*4 + 3] = b3;

      out2[i*4] = b0;
      out2[i*4 + 1] = b1;
      out2[i*4 + 2] = b4;
      out2[i*4 + 3] = b5;

      out3[i*4] = b2;
      out3[i*4 + 1] = b1;
      out3[i*4 + 2] = b4;
      out3[i*4 + 3] = b5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
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
   
      if (out3[i*4] != b2  
         || out3[i*4 + 1] != b1
         || out3[i*4 + 2] != b4
         || out3[i*4 + 3] != b5)
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 + 5;
      b4 = a4 + 4;
      b5 = a5 + 3;
      b6 = a6 + 2;
      b7 = a7 + 1;
      b8 = a8 + 9;

      out[i*4] = b0;
      out[i*4 + 1] = b1;
      out[i*4 + 2] = b2;
      out[i*4 + 3] = b3;

      out2[i*8] = b0;
      out2[i*8 + 1] = b1;
      out2[i*8 + 2] = b4;
      out2[i*8 + 3] = b5;
      out2[i*8 + 4] = b6;
      out2[i*8 + 5] = b2;
      out2[i*8 + 6] = b7;
      out2[i*8 + 7] = b8;

      out3[2*i + 1] = a0;
      out3[2*i] = b8; 
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (out[i*4] != b0 
         || out[i*4 + 1] != b1
         || out[i*4 + 2] != b2
         || out[i*4 + 3] != b3)
        abort ();

      if (out2[i*8] != b0
         || out2[i*8 + 1] != b1
         || out2[i*8 + 2] != b4
         || out2[i*8 + 3] != b5
         || out2[i*8 + 4] != b6
         || out2[i*8 + 5] != b2
         || out2[i*8 + 6] != b7
         || out2[i*8 + 7] != b8)
        abort ();

      if (out3[2*i] != b8
          || out3[2*i+1] != a0)
        abort();
    }


  return 0;
}

int main (void)
{
  check_vect ();

  main1 (8,7,6,5,4,3,2,1,0);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 6 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
