/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 4 / 16)
#else
#define N 32
#endif

unsigned short in[N] = {};
unsigned short in2[N] = {};
unsigned short in3[N] = {};

int
main1 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      in[i] = in2[i] = in3[i] = i;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N/4; i++)
    {
      in[i*4] = in[i*4] + 5;
      in[i*4 + 1] = in[i*4 + 1] + 5;
      in[i*4 + 2] = in[i*4 + 2] + 5;
      in[i*4 + 3] = in[i*4 + 3] + 5;

    }

  /* check results:  */
#pragma GCC novector
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
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (in2[i] != (i % 4) + (i / 4) * 5)
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
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (in3[i] != (i % 12) + (i / 12) * 5)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! vect32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target vect32 } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { ! vect32 } } } } */
  
