/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define M 10
#define N 3

void __attribute__((noinline))
foo (int n, int *ub, int *uc)
{
  int i, j, tmp1;

  for (i = 0; i < n; i++)
    {
      tmp1 = 0;
      for (j = 0; j < M; j++)
        {
          tmp1 += uc[i] * ((int)(j << N) / M);
        }
      ub[i] = tmp1;
    }
}

int main()
{
  int uc[16], ub[16];
  check_vect ();
  foo (16, uc, ub);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_int_mult } } } */
