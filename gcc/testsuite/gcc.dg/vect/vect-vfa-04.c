/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
int result[] = {10, 11, 15, 16, 20, 21, 25, 26, 30, 31, 35, 36, 40, 41, 45, 46, 50, 51};
int X[] =      {10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0, 0};
 
__attribute__ ((noinline)) void
foo (int *in, int *out)
{  
  int i;
  
  for (i = 0; i < N; i++)
    out[i] = in[i] + 5;
}

int
main (void)
{ 
  int i;

  check_vect ();

  foo (X, &X[2]);
  
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N+2; i++)
    {
      if (X[i] != result[i])
	abort ();
    }
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
