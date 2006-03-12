/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct s{
  int m;
  int n[N][N][N];
};

struct test1{
  struct s a; /* array a.n is unaligned */
  int b;
  int c;
  struct s e[N]; /* array e.n is aligned */
};

int main1 ()
{
  int i,j;
  struct test1 tmp1;

  for (i = 0; i < N; i++)
    for (j = 3; j < N-3; j++)
      {
        tmp1.e[i].n[1][2][j] = 8;
      }

  /* check results:  */
  for (i = 0; i < N; i++)
    for (j = 3; j < N-3; j++)
    {
      if (tmp1.e[i].n[1][2][j] != 8)
          abort ();
    }
  
  /* not consecutive */
  for (i = 0; i < N; i++)
    for (j = 3; j < N-3; j++)
      { 
        tmp1.e[j].n[1][2][j] = 8;
      }
  
  /* check results:  */
  for (i = 0; i < N; i++)
    for (j = 3; j < N-3; j++)
    {
      if (tmp1.e[j].n[1][2][j] != 8)
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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
