/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 16

extern void abort (void);

int A[N] = {36, 39, 42, 45, 43, 32, 21, 12, 23, 34, 45, 56, 67, 78, 81, 11};
int B[N] = {144,195,210,225,172,128,105,60, 92, 136,225,280,268,390,324,55};

__attribute__((noinline))
void foo ()
{
  for (int i = 0; i < N; i++)
    {
      int m = (A[i] & i) ? 5 : 4;
      A[i] = A[i] * m;
    }
}

int main ()
{

  check_vect ();
  foo ();
  /* check results:  */
#pragma GCC novector
  for (int i = 0; i < N; i++)
    if (A[i] != B[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
