/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 11 

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

  /* 4. unaligned */
  for (i = 3; i < N-3; i++)
    {
      tmp1.e.n[1][2][i] = 8;
    }
 
  /* check results:  */
  for (i = 3; i <N-3; i++)
    {
      if (tmp1.e.n[1][2][i] != 8)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorization not profitable" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
