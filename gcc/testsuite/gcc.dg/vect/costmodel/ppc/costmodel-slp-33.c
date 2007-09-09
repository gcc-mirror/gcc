/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 32

struct s{
  short a;	/* aligned */
  char b[N-1];  /* unaligned (offset 2B) */
};
 
int main1 ()
{  
  int i;
  struct s tmp;

  /* unaligned */
  for (i = 0; i < N/4; i++)
    {
      tmp.b[2*i] = 5;
      tmp.b[2*i+1] = 15;
    }

  /* check results:  */
  for (i = 0; i <N/4; i++)
    {
      if (tmp.b[2*i] != 5
          || tmp.b[2*i+1] != 15)
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
