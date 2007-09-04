/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

/* indirect access.  */

__attribute__ ((noinline))
int main1 ()
{
  int i;
  unsigned ia[N];
  unsigned ib[N+1] = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2};

  for (i = 2; i < N+1; i++)
    {
      ia[ib[i]] = 0;
    }

  /* check results:  */
  for (i = 2; i < N+1; i++)
    {
      if (ia[ib[i]] != 0)
        abort();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
