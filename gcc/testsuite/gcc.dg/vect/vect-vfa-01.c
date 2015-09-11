/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
int result[N] = {12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27};
int X[N] = {10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25};
int Y[N] = {};
 
__attribute__ ((noinline)) void
foo (int *in, int *out)
{  
  int i;
  
  for (i = 0; i < N; i++)
    out[i] = in[i] + 2;
}

int
main (void)
{ 
  int i;

  check_vect ();

  foo (X, Y);
  
  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (Y[i] != result[i])
	abort ();
    }
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
