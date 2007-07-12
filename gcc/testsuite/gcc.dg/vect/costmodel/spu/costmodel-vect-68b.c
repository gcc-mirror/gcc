/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 32

struct s{
  int m;
  int n[N][N][N];
};

struct test1{
  struct s a; /* array a.n is unaligned */
  int b;
  int c;
  struct s e; /* array e.n is aligned */
};

int main1 ()
{  
  int i,j;
  struct test1 tmp1;

  /* 2. aligned */
  for (i = 3; i < N-1; i++)
    {
      tmp1.a.n[1][2][i] = 6;
    }

  /* check results:  */
  for (i = 3; i < N-1; i++)
    {
      if (tmp1.a.n[1][2][i] != 6)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
