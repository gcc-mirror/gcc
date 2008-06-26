/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 5 

struct t{
  int k[N];
  int l; 
};
  
struct s{
  char a;	/* aligned */
  char b[N-1];  /* unaligned (offset 1B) */
  char c[N];    /* aligned (offset NB) */
  struct t d;   /* aligned (offset 2NB) */
  struct t e;   /* unaligned (offset 2N+4N+4 B) */
};

int main1 ()
{  
  int i;
  struct s tmp;

  /* unaligned */
  for (i = 0; i < N; i++)
    {
      tmp.e.k[i] = 8;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (tmp.e.k[i] != 8)
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
