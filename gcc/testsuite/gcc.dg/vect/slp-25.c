/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* Unaligned stores.  */

int ia[N+1];
short sa[N+1];

int main1 (int n)
{
  int i;

  for (i = 1; i <= N/2; i++)
    {
      ia[2*i] = 25;
      ia[2*i + 1] = 5;
    }

  /* check results:  */
  for (i = 1; i <= N/2; i++)
    {
      if (ia[2*i] != 25
          || ia[2*i + 1] != 5)
        abort ();
    }

  for (i = 1; i <= n/2; i++)
    {
      sa[2*i] = 25;
      sa[2*i + 1] = 5;
    }

  /* check results:  */
  for (i = 1; i <= n/2; i++)
    {
      if (sa[2*i] != 25
          || sa[2*i + 1] != 5)
        abort ();
    }


  return 0;
}

int main (void)
{ 

  check_vect ();
  
  return main1 (N);
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail vect_hw_misalign } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target vect_hw_misalign } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" { xfail { vect_no_align || vect_hw_misalign } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
