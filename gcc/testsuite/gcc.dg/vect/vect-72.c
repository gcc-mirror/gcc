/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* unaligned load.  */

char ia[N];
char ib[N+1];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  for (i=0; i < N+1; i++)
    {
      ib[i] = i;
      /* Avoid vectorization.  */
      if (i%3 == 0)
        ib[i] = 5;
    }

  for (i = 1; i < N+1; i++)
    {
      ia[i-1] = ib[i];
    }

  /* check results:  */
  for (i = 1; i <= N; i++)
    {
      if (ia[i-1] != ib[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
