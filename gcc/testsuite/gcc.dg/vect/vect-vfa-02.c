/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
int resultY[N] = {12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27};
int resultZ[N] = {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28};
int X[N] = {10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25};
int Y[N] = {};
int Z[N] = {};
 
__attribute__ ((noinline)) void
foo (int *in, int *out1, int *out2)
{
  int i;

  for (i = 0; i < N; i++)
    {
      out1[i] = in[i] + 2;
      out2[i] = in[i] + 3;
    }
}

int
main (void)
{ 
  int i;

  check_vect ();

  foo (X, Y, Z);
  
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (Y[i] != resultY[i])
	abort ();

      if (Z[i] != resultZ[i])
	abort ();
    }
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
