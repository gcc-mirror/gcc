/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32 

unsigned short in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
unsigned short in2[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
unsigned short in3[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
unsigned short check[N] = {0,1,2,3,5,6,7,8,10,11,12,13,15,16,17,18,20,21,22,23,25,26,27,28,30,31,32,33,35,36,37,38};
unsigned short check3[N] = {0,1,2,3,4,5,6,7,8,9,10,11,5,6,7,8,9,10,11,12,13,14,15,16,10,11,12,13,14,15,16,17};

int
main1 ()
{
  int i;

  for (i = 0; i < N/4; i++)
    {
      in[i*4] = in[i*4] + 5;
      in[i*4 + 1] = in[i*4 + 1] + 5;
      in[i*4 + 2] = in[i*4 + 2] + 5;
      in[i*4 + 3] = in[i*4 + 3] + 5;

    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (in[i] != i+5)
        abort ();
    }

  /* Not vectorizable because of data dependencies.  */
  for (i = 1; i < N/4; i++)
    {
      in2[i*4] = in2[(i-1)*4] + 5;
      in2[i*4 + 1] = in2[(i-1)*4 + 1] + 5;
      in2[i*4 + 2] = in2[(i-1)*4 + 2] + 5;
      in2[i*4 + 3] = in2[(i-1)*4 + 3] + 5;

    }

  /* check results:  */
  for (i = 4; i < N; i++)
    {
      if (in2[i] != check[i])
        abort ();
    }
  
  /* Not vectorizable because of data dependencies: distance 3 is greater than 
     the actual VF with SLP (2), but the analysis fail to detect that for now.  */
  for (i = 3; i < N/4; i++)
    {
      in3[i*4] = in3[(i-3)*4] + 5;
      in3[i*4 + 1] = in3[(i-3)*4 + 1] + 5;
      in3[i*4 + 2] = in3[(i-3)*4 + 2] + 5;
      in3[i*4 + 3] = in3[(i-3)*4 + 3] + 5;

    }

  /* check results:  */
  for (i = 12; i < N; i++)
    {
      if (in3[i] != check3[i])
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  } } */
  
