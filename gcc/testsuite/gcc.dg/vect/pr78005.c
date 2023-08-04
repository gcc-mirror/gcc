/* { dg-require-effective-target vect_int } */
#include "tree-vect.h"

#define N 20
int u[N] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
int z[N] = {-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,  10, 11, 12, 13, 14, 15, 16, 17, 18};
int res4[N] = {0, 1, 8, 3, 22, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
int res5[N] = {0, 1, 8, 3, 22, 5, 36, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
int res6[N] = {0, 1, 8, 3, 22, 5, 36, 7, 50, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
int res7[N] = {0, 1, 8, 3, 22, 5, 36, 7, 50, 9, 64, 11, 12, 13, 14, 15, 16, 17, 18, 19};
int res8[N] = {0, 1, 8, 3, 22, 5, 36, 7, 50, 9, 64, 11, 78, 13, 14, 15, 16, 17, 18, 19};
int res9[N] = {0, 1, 8, 3, 22, 5, 36, 7, 50, 9, 64, 11, 78, 13, 92, 15, 16, 17, 18, 19};
int res10[N] = {0, 1, 8, 3, 22, 5, 36, 7, 50, 9, 64, 11, 78, 13, 92, 15, 106, 17, 18, 19};

__attribute__ ((noinline)) void
foo (int n, int d)
{
  int i;
  for (i = 2; i < n; i++)
    u[2*i-2] = u[2*i-2] + d * (z[i-1] + z[i] + z[i-1] + z[i] + z[i-1] + z[i]);
}

#define check_u(x)		\
  foo (x, 2);			\
  _Pragma("GCC novector")	\
  for (i = 0; i < N; i++)	\
    {				\
      if (u[i] != res##x[i])	\
	abort ();		\
      u[i] = i;			\
    }

int main(void)
{
  int i;

  check_vect ();

  /* Need to check for all possible vector factors.  */
  check_u(4);
  check_u(5);
  check_u(6);
  check_u(7);
  check_u(8);
  check_u(9);
  check_u(10);

  return 0;
}
