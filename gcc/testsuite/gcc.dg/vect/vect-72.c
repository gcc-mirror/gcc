/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* unaligned load.  */

int main1 ()
{
  int i;
  char ia[N];
  char ib[N+1];

  for (i=0; i < N+1; i++)
    {
      ib[i] = i;
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

