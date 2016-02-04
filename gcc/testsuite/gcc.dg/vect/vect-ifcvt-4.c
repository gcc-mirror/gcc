/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define MAX 42

int A[N] = {36,39,42,45,43,32,21,12,23,34,45,56,67,78,89,11};
int B[N] = {42,42,42,0,0,42,42,42,42,42,0,0,0,0,0,42};

extern void abort(void); 

int main ()
{  
  int i, j;

  check_vect ();

  for (i = 0; i < 16; i++)
    A[i] = ( A[i] <= MAX ? MAX : 0); 

  /* check results:  */
  for (i = 0; i < N; i++)
    if (A[i] != B[i])
      abort ();

  return 0;
}



/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
